extern crate libc;

// mod editor;
mod lib;
// mod ncurses;
mod script;

use script::rt::{Gc, Ref, Value};
use script::tp::{Native, Scope, Seq};
use std::result;

#[derive(Debug)]
pub enum Error {
    Script(script::Error),
    NotTask(Ref),
}

impl From<script::Error> for Error {
    fn from(err: script::Error) -> Self {
        Error::Script(err)
    }
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Debug)]
pub enum Task {
    Version,
    Print(Ref),
}

impl Value for Task {
    fn mark_rec(&self, gc: &mut Gc) {
        match self {
            Task::Version => {}
            Task::Print(r) => gc.mark(*r),
        }
    }
}

pub struct Rim {
    rt: script::Runtime,
    root: Ref,
}

impl Rim {
    fn new() -> Self {
        let mut rt = script::Runtime::new();

        let root = {
            let mut mem = rt.memory_mut();
            mem.debug(true);

            let mut host = Scope::new();
            host.insert("version", mem.alloc(Task::Version));
            host.insert(
                "print",
                mem.alloc(Native::new(|mem, v| mem.alloc(Task::Print(v)))),
            );
            let host = mem.alloc(host);

            let mut root = Scope::new();
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
            while let Some(seq) = self.rt.memory().get::<Seq>(val) {
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
                let mut gc = Gc::new(self.rt.memory());
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

    fn root(&self) -> &Scope {
        self.rt
            .memory()
            .get::<Scope>(self.root)
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
