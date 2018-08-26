pub mod ast;
pub mod parser;
pub mod rt;
pub mod tp;

use self::rt::{Memory, Ref};
use self::tp::{Func, Native, Scope, Seq, Struct, Type};
use std::{fs, io, path, result};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parser(parser::Error),
    NonCallable(Ref),
    NonScope(Ref),
    NonStruct(Ref),
    NoMember(Scope, String),
    PatternFail(Ref),
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
    mem: Memory,
}

impl Runtime {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn memory(&self) -> &Memory {
        &self.mem
    }

    pub fn memory_mut(&mut self) -> &mut Memory {
        &mut self.mem
    }

    pub fn load(&mut self, content: &str, env: Ref) -> Result<()> {
        let decls = parser::parse(&content)?;

        for decl in decls {
            match decl.kind {
                ast::DeclKind::Type(tp) => {
                    for var in tp.variants {
                        let tp = Type {
                            name: var.name.clone(),
                            fields: var.fields,
                        };
                        let tp = self.mem.alloc(tp);

                        let st = Struct {
                            tp,
                            fields: Vec::new(),
                        };
                        let r = self.mem.alloc(st);

                        let scope = self.mem.get_mut::<Scope>(env).expect("load env not scope");
                        scope.insert(var.name, r);
                    }
                }
                ast::DeclKind::Bind(expr) => {
                    let r = self.eval(&expr, env)?;

                    let scope = self.mem.get_mut::<Scope>(env).expect("load env not scope");
                    scope.insert(decl.name, r);
                }
            };
        }
        Ok(())
    }

    pub fn load_file<P>(&mut self, path: P, env: Ref) -> Result<()>
    where
        P: AsRef<path::Path>,
    {
        let content = fs::read_to_string(path)?;
        self.load(&content, env)
    }

    pub fn fetch(&self, name: &str, scope: &Scope) -> Result<Ref> {
        if let Some(r) = scope.get(name) {
            Ok(r)
        } else if let Some(p) = scope.parent() {
            let p = self.mem.get::<Scope>(p).expect("scope parent not scope");
            self.fetch(name, p)
        } else {
            Err(Error::NoMember(scope.clone(), name.to_string()))
        }
    }

    pub fn call(&mut self, func: Ref, arg: Ref) -> Result<Ref> {
        if let Some(native) = self.mem.get::<Native>(func).cloned() {
            Ok(native.call(&mut self.mem, arg))
        } else if let Some(func) = self.mem.get::<Func>(func).cloned() {
            let mut scope = Scope::from(func.env);
            if !self.pattern(&func.param, arg, func.env, &mut scope)? {
                return Err(Error::PatternFail(arg));
            }
            let env = self.mem.alloc(scope);

            self.eval(&func.body, env)
        } else if let Some(mut var) = self.mem.get::<Struct>(func).cloned() {
            {
                let tp = self.mem.get::<Type>(var.tp).expect("struct type not type");
                if var.fields.len() == tp.fields.len() {
                    return Err(Error::NonCallable(func));
                }
            }
            var.fields.push(arg);
            Ok(self.mem.alloc(var))
        } else {
            Err(Error::NonCallable(func))
        }
    }

    pub fn eval(&mut self, expr: &ast::Expr, env: Ref) -> Result<Ref> {
        match &expr.kind {
            ast::ExprKind::Unit => Ok(self.mem.alloc(())),
            ast::ExprKind::String(s) => Ok(self.mem.alloc(s.clone())),
            ast::ExprKind::Bind(bind) => self.get_bind(bind, env),
            ast::ExprKind::Apply(func, arg) => {
                let func = self.eval(&func, env)?;
                let arg = self.eval(&arg, env)?;
                self.call(func, arg)
            }
            ast::ExprKind::Func(param, body) => {
                let param = param.clone();
                let body = (**body).clone();
                Ok(self.mem.alloc(Func { param, body, env }))
            }
            ast::ExprKind::Seq(expr, next) => {
                let task = self.eval(&expr, env)?;
                let next = self.eval(&next, env)?;
                Ok(self.mem.alloc(Seq { task, next }))
            }
            ast::ExprKind::Match(expr, arms) => {
                let val = self.eval(&expr, env)?;
                for arm in arms.iter() {
                    let mut scope = Scope::from(env);
                    if self.pattern(&arm.pattern, val, env, &mut scope)? {
                        let env = self.mem.alloc(scope);
                        return self.eval(&arm.value, env);
                    }
                }
                Err(Error::PatternFail(val))
            }
        }
    }

    fn pattern(&self, pat: &ast::Pattern, val: Ref, env: Ref, scope: &mut Scope) -> Result<bool> {
        match &pat.kind {
            ast::PatternKind::Bind(name) => scope.insert(name.clone(), val),
            ast::PatternKind::Struct(var, fields) => {
                let var = self.get_bind(var, env)?;
                let tp = self
                    .mem
                    .get::<Struct>(var)
                    .ok_or_else(|| Error::NonStruct(var))?;
                let val = self
                    .mem
                    .get::<Struct>(val)
                    .ok_or_else(|| Error::NonStruct(val))?;

                if val.tp != tp.tp {
                    return Ok(false);
                }
                for (pat, &field) in fields.iter().zip(&val.fields) {
                    if !self.pattern(pat, field, env, scope)? {
                        return Ok(false);
                    }
                }
            }
            ast::PatternKind::Ignore => {}
        }

        Ok(true)
    }

    fn get_bind(&self, bind: &ast::Bind, env: Ref) -> Result<Ref> {
        let mut r = env;
        for name in &bind.names {
            let scope = self.mem.get::<Scope>(r).ok_or_else(|| Error::NonScope(r))?;
            r = self.fetch(&name, scope)?
        }
        Ok(r)
    }
}
