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

pub struct Cont {
    pub task: Task,
    pub kind: ContKind,
}

pub enum ContKind {
    Cont(rt::Ref),
    End,
}

#[derive(Default)]
pub struct Script {
    rt: rt::Runtime,
    env: rt::Scope,
}

impl Script {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn step(&mut self, func: rt::Ref, arg: rt::Ref) -> Result<Cont> {
        let ret = self.rt.call(func, arg)?;
        let (task, kind) = if let Some(rt::Seq { task, next }) = self.rt.get::<rt::Seq>(ret) {
            (*task, ContKind::Cont(*next))
        } else {
            (ret, ContKind::End)
        };

        let task = self
            .rt
            .get::<Task>(task)
            .ok_or_else(|| Error::NotTask(task))?
            .clone();
        Ok(Cont { task, kind })
    }

    pub fn load(&mut self, content: &str) -> Result<()> {
        let decls = parser::parse(&content)?;

        self.rt.insert("version", Task::Version, &mut self.env);
        self.rt.insert(
            "print",
            rt::Native::new(|rt, v| Ok(rt.alloc(Task::Print(v)))),
            &mut self.env,
        );

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

        loop {
            let cont = self.step(func, arg)?;

            match cont.task {
                Task::Version => arg = self.rt.alloc("0.1".to_string()),
                Task::Print(val) => println!("{:?}", self.rt.get_any(val)),
            }

            match cont.kind {
                ContKind::Cont(cont) => {
                    func = cont;
                }
                ContKind::End => break,
            }
        }

        self.rt.dump();
        Ok(())
    }
}
