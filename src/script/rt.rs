use super::ast;
use lib::Store;
use std::collections::HashMap;
use std::rc::Rc;
use std::{any, fmt, mem};

pub trait AsAny: 'static {
    fn as_any(&self) -> &any::Any;

    fn as_any_mut(&mut self) -> &mut any::Any;
}

impl<T: any::Any> AsAny for T {
    fn as_any(&self) -> &any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut any::Any {
        self
    }
}

pub trait Value: AsAny + fmt::Debug {
    fn mark_rec(&self, _gc: &mut Gc) {}
}

impl Value for () {}

impl Value for String {}

#[derive(Clone, Copy)]
pub struct Ref {
    index: usize,
}

impl fmt::Debug for Ref {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.index)
    }
}

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
        write!(f, "Scope {:?}", self.binds)?;
        if let Some(parent) = self.parent {
            write!(f, " | {:?}", parent)?;
        }
        Ok(())
    }
}

impl Value for Scope {
    fn mark_rec(&self, gc: &mut Gc) {
        for &r in self.binds.values() {
            gc.mark(r);
            if let Some(parent) = self.parent {
                gc.mark(parent);
            }
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
        write!(f, "Func /{} {} | {:?}", self.param, self.body, self.env)
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

#[derive(Default)]
pub struct Memory {
    store: Store<Box<Value>>,
}

impl Memory {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn debug(&mut self, debug: bool) {
        self.store.reuse(!debug);
    }

    pub fn alloc<T>(&mut self, val: T) -> Ref
    where
        T: Value,
    {
        let index = self.store.add(Box::new(val));
        Ref { index }
    }

    pub fn gc(&mut self, result: &GcResult) {
        for (i, mark) in result.mark.iter().enumerate() {
            if !mark {
                self.store.remove(i);
            }
        }
    }

    pub fn dump(&self) {
        let in_use = self.store.len();
        let total = self.store.capacity();
        println!("Memory dump: {} in use, {} free", in_use, total - in_use);
        for (i, cell) in self.store.iter() {
            println!("{:4}: {:?}", i, cell);
        }
    }

    pub fn get_any(&self, r: Ref) -> &Value {
        &**self
            .store
            .get(r.index)
            .expect("attemp to access freed object")
    }

    pub fn get<T>(&self, r: Ref) -> Option<&T>
    where
        T: Value,
    {
        self.get_any(r).as_any().downcast_ref::<T>()
    }

    pub fn get_any_mut(&mut self, r: Ref) -> &mut Value {
        &mut **self
            .store
            .get_mut(r.index)
            .expect("attemp to access freed object")
    }

    pub fn get_mut<T>(&mut self, r: Ref) -> Option<&mut T>
    where
        T: Value,
    {
        self.get_any_mut(r).as_any_mut().downcast_mut::<T>()
    }
}

pub struct Gc<'a> {
    memory: &'a Memory,
    result: GcResult,
}

impl<'a> Gc<'a> {
    pub fn new(memory: &'a Memory) -> Self {
        let mut mark = Vec::new();
        mark.resize(memory.store.capacity(), true);
        for i in memory.store.keys() {
            mark[i] = false;
        }

        let result = GcResult { mark };
        Gc { memory, result }
    }

    pub fn mark(&mut self, r: Ref) {
        if let Some(val) = self.memory.store.get(r.index) {
            if !mem::replace(&mut self.result.mark[r.index], true) {
                val.mark_rec(self);
            }
        }
    }

    pub fn run(self) -> GcResult {
        self.result
    }
}

pub struct GcResult {
    mark: Vec<bool>,
}
