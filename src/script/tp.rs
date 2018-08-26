use super::ast;
use super::rt::{Gc, Memory, Ref, Value};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

impl Value for () {}

impl Value for String {}

#[derive(Clone, Default)]
pub struct Scope {
    binds: HashMap<String, Ref>,
    parent: Option<Ref>,
}

impl Scope {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn from(parent: Ref) -> Self {
        Scope {
            binds: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: &str) -> Option<Ref> {
        self.binds.get(name).cloned()
    }

    pub fn parent(&self) -> Option<Ref> {
        self.parent
    }

    pub fn insert<Str>(&mut self, name: Str, r: Ref)
    where
        Str: Into<String>,
    {
        self.binds.insert(name.into(), r);
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Scope")?;
        if let Some(parent) = self.parent {
            write!(f, " {:?}", parent)?;
        }
        write!(f, " {:?}", self.binds)
    }
}

impl Value for Scope {
    fn mark_rec(&self, gc: &mut Gc) {
        if let Some(parent) = self.parent {
            gc.mark(parent);
        }
        for &r in self.binds.values() {
            gc.mark(r);
        }
    }
}

#[derive(Clone, Debug)]
pub struct Type {
    pub name: String,
    pub fields: Vec<String>,
}

impl Value for Type {}

#[derive(Clone, Debug)]
pub struct Struct {
    pub tp: Ref,
    pub fields: Vec<Ref>,
}

impl Value for Struct {
    fn mark_rec(&self, gc: &mut Gc) {
        gc.mark(self.tp);
        for &field in &self.fields {
            gc.mark(field);
        }
    }
}

#[derive(Clone, Debug)]
pub struct Seq {
    pub task: Ref,
    pub next: Ref,
}

impl Value for Seq {
    fn mark_rec(&self, gc: &mut Gc) {
        gc.mark(self.task);
        gc.mark(self.next);
    }
}

#[derive(Clone)]
pub struct Func {
    pub param: ast::Pattern,
    pub body: ast::Expr,
    pub env: Ref,
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Func {:?} /{} {}", self.env, self.param, self.body)
    }
}

impl Value for Func {
    fn mark_rec(&self, gc: &mut Gc) {
        gc.mark(self.env);
    }
}

#[derive(Clone)]
pub struct Native {
    func: Rc<Fn(&mut Memory, Ref) -> Ref>,
}

impl Native {
    pub fn new<F>(f: F) -> Self
    where
        F: Fn(&mut Memory, Ref) -> Ref + 'static,
    {
        Native { func: Rc::new(f) }
    }

    pub fn call(&self, mem: &mut Memory, r: Ref) -> Ref {
        (self.func)(mem, r)
    }
}

impl fmt::Debug for Native {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Native")
    }
}

impl Value for Native {}
