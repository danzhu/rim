mod ast;
mod parser;
mod rt;

use std::io::prelude::*;
use std::{fs, io, path, result};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse(parser::Error),
    Runtime(rt::Error),
    MissingMain,
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
        Error::Runtime(err)
    }
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Debug)]
pub enum Task {
    Version,
    Print(rt::Ref),
}

impl rt::Value for Task {}

pub struct Script {
    rt: rt::Runtime,
    env: rt::Scope,
}

impl Script {
    pub fn new() -> Self {
        let mut rt = rt::Runtime::new();
        let mut env = rt::Scope::new();
        let mut host = rt::Scope::new();

        rt.insert("version", Task::Version, &mut host);
        rt.insert(
            "print",
            rt::Native::new(|rt, v| Ok(rt.alloc(Task::Print(v)))),
            &mut host,
        );

        rt.insert("rim", host, &mut env);

        Script { rt, env }
    }

    pub fn runtime(&self) -> &rt::Runtime {
        &self.rt
    }

    pub fn load(&mut self, content: &str) -> Result<()> {
        let decls = parser::parse(&content)?;

        self.rt.load(decls, &mut self.env)?;
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
        let mut func = self.env.get("main").ok_or(Error::MissingMain)?;
        let mut arg = self.rt.alloc(());
        let mut cont = Vec::new();

        loop {
            let mut val = self.rt.call(func, arg)?;
            while let Some(seq) = self.rt.get::<rt::Seq>(val) {
                cont.push(seq.next);
                val = seq.task;
            }

            let task = self
                .rt
                .get::<Task>(val)
                .ok_or_else(|| Error::NotTask(val))?
                .clone();

            match task {
                Task::Version => arg = self.rt.alloc("0.1".to_string()),
                Task::Print(val) => println!("{:?}", self.rt.get_any(val)),
            }

            match cont.pop() {
                Some(next) => func = next,
                None => break,
            }
        }

        self.rt.dump();
        Ok(())
    }
}

impl Default for Script {
    fn default() -> Self {
        Script::new()
    }
}
