extern crate libc;

// mod editor;
mod lib;
// mod ncurses;
mod script;

use script::rt;
use std::result;

#[derive(Debug)]
pub enum Error {
    Memory(script::Error),
    NotTask(rt::Ref),
}

impl From<script::Error> for Error {
    fn from(err: script::Error) -> Self {
        Error::Memory(err)
    }
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Debug)]
pub enum Task {
    Version,
    Print(rt::Ref),
}

impl rt::Value for Task {
    fn mark_rec(&self, gc: &mut rt::Gc) {
        match self {
            Task::Version => {}
            Task::Print(r) => gc.mark(*r),
        }
    }
}

pub struct Rim {
    rt: script::Runtime,
    root: rt::Ref,
}

impl Rim {
    fn new() -> Self {
        let mut rt = script::Runtime::new();

        let root = {
            let mut mem = rt.memory_mut();
            mem.debug(true);

            let mut host = rt::Scope::new();
            host.insert("version", mem.alloc(Task::Version));
            host.insert(
                "print",
                mem.alloc(rt::Native::new(|mem, v| mem.alloc(Task::Print(v)))),
            );
            let host = mem.alloc(host);

            let mut root = rt::Scope::new();
            root.insert("rim", host);
            mem.alloc(root)
        };

        Rim { rt, root }
    }

    fn run(&mut self) -> Result<()> {
        self.rt.load_file("test.fin", self.root)?;

        let mut val = self.rt.fetch("main", self.root())?;
        let mut conts = Vec::new();

        loop {
            while let Some(seq) = self.rt.memory().get::<rt::Seq>(val) {
                conts.push(seq.next);
                val = seq.task;
            }

            let task = self
                .rt
                .memory()
                .get::<Task>(val)
                .ok_or_else(|| Error::NotTask(val))?
                .clone();

            let arg = match task {
                Task::Version => self.rt.memory_mut().alloc("0.1".to_string()),
                Task::Print(val) => {
                    println!("{:?}", self.rt.memory().get_any(val));
                    self.rt.memory_mut().alloc(())
                }
            };

            let func = match conts.pop() {
                Some(func) => func,
                None => break,
            };
            val = self.rt.call(func, arg)?;

            let result = {
                let mut gc = rt::Gc::new(self.rt.memory());
                gc.mark(self.root);
                gc.mark(val);
                for &cont in &conts {
                    gc.mark(cont);
                }
                gc.run()
            };
            self.rt.memory_mut().gc(&result);
        }

        self.rt.memory().dump();

        Ok(())
    }

    fn root(&self) -> &rt::Scope {
        self.rt
            .memory()
            .get::<rt::Scope>(self.root)
            .expect("root not scope")
    }
}

// fn run() -> ncurses::Result<()> {
//     ncurses::initscr()?;
//     ncurses::cbreak()?;
//     ncurses::keypad(true)?;
//     ncurses::noecho()?;

//     ncurses::getch()?;

//     ncurses::endwin()
// }

fn main() {
    let mut rim = Rim::new();
    match rim.run() {
        Ok(_) => {}
        Err(err) => {
            rim.rt.memory().dump();
            println!("failed to run: {:?}", err);
        }
    }
}
