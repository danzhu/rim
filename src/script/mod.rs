mod ast;
mod parser;
pub mod rt;

use std::io::prelude::*;
use std::{fs, io, path, result};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parser(parser::Error),
    NonCallable(rt::Ref),
    NonScope(rt::Ref),
    NoMember(rt::Scope, String),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Self {
        Error::Parser(err)
    }
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Default)]
pub struct Runtime {
    mem: rt::Memory,
}

impl Runtime {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn memory(&self) -> &rt::Memory {
        &self.mem
    }

    pub fn memory_mut(&mut self) -> &mut rt::Memory {
        &mut self.mem
    }

    pub fn load(&mut self, content: &str, env: rt::Ref) -> Result<()> {
        let decls = parser::parse(&content)?;

        for decl in decls {
            let r = self.eval(&decl.value, env)?;

            let scope = self
                .mem
                .get_mut::<rt::Scope>(env)
                .expect("load env not scope");
            scope.insert(decl.name, r);
        }
        Ok(())
    }

    pub fn load_file<P>(&mut self, path: P, env: rt::Ref) -> Result<()>
    where
        P: AsRef<path::Path>,
    {
        let mut file = fs::File::open(path)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;

        self.load(&content, env)
    }

    pub fn fetch(&self, name: &str, scope: &rt::Scope) -> Result<rt::Ref> {
        if let Some(r) = scope.get(name) {
            Ok(r)
        } else if let Some(p) = scope.parent() {
            let p = self
                .mem
                .get::<rt::Scope>(p)
                .expect("scope parent not scope");
            self.fetch(name, p)
        } else {
            Err(Error::NoMember(scope.clone(), name.to_string()))
        }
    }

    pub fn call(&mut self, func: rt::Ref, arg: rt::Ref) -> Result<rt::Ref> {
        if let Some(native) = self.mem.get::<rt::Native>(func).cloned() {
            Ok(native.call(&mut self.mem, arg))
        } else if let Some(func) = self.mem.get::<rt::Func>(func).cloned() {
            let mut scope = rt::Scope::from(func.env);
            scope.insert(func.param, arg);
            let env = self.mem.alloc(scope);

            self.eval(&func.body, env)
        } else {
            Err(Error::NonCallable(func))
        }
    }

    pub fn eval(&mut self, expr: &ast::Expr, env: rt::Ref) -> Result<rt::Ref> {
        match &expr.kind {
            ast::ExprKind::Bind(bind) => {
                let mut r = env;
                for name in &bind.names {
                    let scope = self
                        .mem
                        .get::<rt::Scope>(r)
                        .ok_or_else(|| Error::NonScope(r))?;
                    r = self.fetch(&name, scope)?
                }
                Ok(r)
            }
            ast::ExprKind::Apply(func, arg) => {
                let func = self.eval(&func, env)?;
                let arg = self.eval(&arg, env)?;
                self.call(func, arg)
            }
            ast::ExprKind::Func(param, body) => {
                let param = param.clone();
                let body = (**body).clone();
                Ok(self.mem.alloc(rt::Func { param, body, env }))
            }
            ast::ExprKind::Seq(expr, next) => {
                let task = self.eval(&expr, env)?;
                let next = self.eval(&next, env)?;
                Ok(self.mem.alloc(rt::Seq { task, next }))
            }
        }
    }
}
