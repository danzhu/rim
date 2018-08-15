mod ast;
mod parser;
mod rt;

use std::io::prelude::*;
use std::{fs, io, path, result};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse(parser::Error),
    Memory(rt::Error),
    NotTask(rt::Ref),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Self {
        Error::Parse(err)
    }
}

impl From<rt::Error> for Error {
    fn from(err: rt::Error) -> Self {
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

pub struct Script {
    mem: rt::Memory,
    env: rt::Ref,
}

impl Script {
    pub fn new() -> Self {
        let mut mem = rt::Memory::new();
        let mut scope = rt::Scope::new();
        let mut host = rt::Scope::new();

        mem.debug(true);

        mem.insert("version", Task::Version, &mut host);
        mem.insert(
            "print",
            rt::Native::new(|mem, v| Ok(mem.alloc(Task::Print(v)))),
            &mut host,
        );

        mem.insert("rim", host, &mut scope);

        let env = mem.alloc(scope);

        Script { mem, env }
    }

    pub fn memory(&self) -> &rt::Memory {
        &self.mem
    }

    pub fn load(&mut self, content: &str) -> Result<()> {
        let decls = parser::parse(&content)?;

        self.mem.load(decls, self.env)?;
        Ok(())
    }

    pub fn load_file<P>(&mut self, path: P) -> Result<()>
    where
        P: AsRef<path::Path>,
    {
        let mut file = fs::File::open(path)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;

        self.load(&content)
    }

    pub fn run(&mut self) -> Result<()> {
        let mut func = self.mem.fetch("main", self.scope())?;
        let mut arg = self.mem.alloc(());
        let mut conts = Vec::new();

        loop {
            let mut ret = self.mem.call(func, arg)?;
            while let Some(seq) = self.mem.get::<rt::Seq>(ret) {
                conts.push(seq.next);
                ret = seq.task;
            }

            let task = self
                .mem
                .get::<Task>(ret)
                .ok_or_else(|| Error::NotTask(ret))?
                .clone();

            arg = match task {
                Task::Version => self.mem.alloc("0.1".to_string()),
                Task::Print(val) => {
                    println!("{:?}", self.mem.get_any(val));
                    self.mem.alloc(())
                }
            };

            let result = {
                let mut gc = rt::Gc::new(&self.mem);
                gc.mark(self.env);
                gc.mark(func);
                gc.mark(arg);
                for &cont in &conts {
                    gc.mark(cont);
                }
                gc.run()
            };
            self.mem.gc(&result);

            match conts.pop() {
                Some(next) => func = next,
                None => break,
            }
        }

        self.mem.dump();

        Ok(())
    }

    fn scope(&self) -> &rt::Scope {
        self.mem.get::<rt::Scope>(self.env).expect("env not scope")
    }
}

impl Default for Script {
    fn default() -> Self {
        Script::new()
    }
}
