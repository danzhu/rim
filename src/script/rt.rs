use super::ast;
use std::collections::HashMap;
use std::rc::Rc;
use std::{any, fmt, result};

pub trait AsAny: 'static {
    fn as_any_imp(&self) -> &any::Any;
}

impl<T: any::Any> AsAny for T {
    fn as_any_imp(&self) -> &any::Any {
        self
    }
}

pub trait Value: AsAny + fmt::Debug {
    fn as_any(&self) -> &any::Any {
        self.as_any_imp()
    }

    fn mark_rec(&self, _gc: &mut Gc) {}
}

impl Value for () {}

impl Value for String {}

#[derive(Clone, Debug)]
pub enum Error {
    NonCallable(Ref),
    NonScope(Ref),
    NoMember(Scope, String),
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Copy)]
pub struct Ref {
    index: usize,
}

impl Ref {}

impl fmt::Debug for Ref {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.index)
    }
}

#[derive(Clone, Default)]
pub struct Scope {
    binds: HashMap<String, Ref>,
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Scope {:?}", self.binds)
    }
}

impl Scope {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, name: &str) -> Option<Ref> {
        self.binds.get(name).cloned()
    }
}

impl Value for Scope {
    fn mark_rec(&self, gc: &mut Gc) {
        for &r in self.binds.values() {
            gc.mark(r);
        }
    }
}

#[derive(Debug)]
struct Cell {
    value: Box<Value>,
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
    param: String,
    body: ast::Expr,
    env: Scope,
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Func /{} {}", self.param, self.body)?;
        write!(f, "    {:?}", self.env)
    }
}

impl Value for Func {
    fn mark_rec(&self, gc: &mut Gc) {
        self.env.mark_rec(gc);
    }
}

#[derive(Clone)]
pub struct Native {
    func: Rc<Fn(&mut Runtime, Ref) -> Result<Ref>>,
}

impl Native {
    pub fn new<F>(f: F) -> Self
    where
        F: Fn(&mut Runtime, Ref) -> Result<Ref> + 'static,
    {
        Native { func: Rc::new(f) }
    }
}

impl fmt::Debug for Native {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Native")
    }
}

impl Value for Native {}

#[derive(Default)]
pub struct Runtime {
    memory: Vec<Option<Cell>>,
    free: Vec<Ref>,
}

impl Runtime {
    pub fn new() -> Self {
        Default::default()
    }

    fn cell(&self, r: Ref) -> &Option<Cell> {
        &self.memory[r.index]
    }

    fn cell_mut(&mut self, r: Ref) -> &mut Option<Cell> {
        &mut self.memory[r.index]
    }

    pub fn alloc<T>(&mut self, val: T) -> Ref
    where
        T: Value,
    {
        let c = Some(Cell {
            value: Box::new(val),
        });

        match self.free.pop() {
            Some(r) => {
                let cell = self.cell_mut(r);
                assert!(cell.is_none(), "free list ref pointing to non-free cell");
                *cell = c;
                r
            }
            None => {
                let index = self.memory.len();
                self.memory.push(c);
                Ref { index }
            }
        }
    }

    pub fn gc(&mut self, result: GcResult) {
        for (index, (cell, mark)) in self.memory.iter_mut().zip(result.mark).enumerate() {
            if !mark && cell.is_some() {
                self.free.push(Ref { index });
                *cell = None;
            }
        }
    }

    pub fn dump(&self) {
        println!("Memory dump:");
        for (i, cell) in self.memory.iter().enumerate() {
            match cell {
                Some(cell) => println!("{}: {:?}", i, cell.value),
                None => {}
            }
        }
    }

    pub fn get_any(&self, r: Ref) -> &Value {
        &*self
            .cell(r)
            .as_ref()
            .expect("attemp to access freed object")
            .value
    }

    pub fn get<T>(&self, r: Ref) -> Option<&T>
    where
        T: Value,
    {
        self.get_any(r).as_any().downcast_ref::<T>()
    }

    pub fn insert<Str, T>(&mut self, name: Str, val: T, env: &mut Scope)
    where
        Str: Into<String>,
        T: Value,
    {
        env.binds.insert(name.into(), self.alloc(val));
    }

    pub fn load(&mut self, decls: Vec<ast::Decl>, env: &mut Scope) -> Result<()> {
        for decl in decls {
            let r = self.eval(&decl.value, env)?;
            env.binds.insert(decl.name, r);
        }
        Ok(())
    }

    pub fn call(&mut self, func: Ref, arg: Ref) -> Result<Ref> {
        if let Some(native) = self.get::<Native>(func).cloned() {
            (native.func)(self, arg)
        } else if let Some(func) = self.get::<Func>(func).cloned() {
            let mut env = func.env;
            env.binds.insert(func.param, arg);

            self.eval(&func.body, &env)
        } else {
            Err(Error::NonCallable(func))
        }
    }

    pub fn eval(&mut self, expr: &ast::Expr, env: &Scope) -> Result<Ref> {
        match &expr.kind {
            ast::ExprKind::Bind(bind) => {
                let mut parent = env;
                for name in bind.names.iter().take(bind.names.len() - 1) {
                    let r = parent
                        .get(name)
                        .ok_or_else(|| Error::NoMember(parent.clone(), name.clone()))?;
                    parent = self.get::<Scope>(r).ok_or_else(|| Error::NonScope(r))?;
                }
                let name = bind.names.last().expect("empty bind");
                let r = parent
                    .get(name)
                    .ok_or_else(|| Error::NoMember(parent.clone(), name.clone()))?;
                Ok(r)
            }
            ast::ExprKind::Apply(func, arg) => {
                let func = self.eval(&func, env)?;
                let arg = self.eval(&arg, env)?;
                self.call(func, arg)
            }
            ast::ExprKind::Func(name, body) => Ok(self.alloc(Func {
                param: name.clone(),
                body: *body.clone(),
                env: env.clone(),
            })),
            ast::ExprKind::Seq(expr, next) => {
                let task = self.eval(&expr, env)?;
                let next = self.eval(&next, env)?;
                Ok(self.alloc(Seq { task, next }))
            }
        }
    }
}

pub struct Gc<'a> {
    runtime: &'a Runtime,
    result: GcResult,
}

impl<'a> Gc<'a> {
    pub fn new(runtime: &'a Runtime) -> Self {
        let mut mark = Vec::new();
        mark.resize(runtime.memory.len(), false);
        let result = GcResult { mark };
        Gc { runtime, result }
    }

    pub fn mark(&mut self, r: Ref) {
        if let Some(cell) = self.runtime.cell(r) {
            self.result.mark[r.index] = true;
            cell.value.mark_rec(self);
        }
    }

    pub fn run(self) -> GcResult {
        self.result
    }
}

pub struct GcResult {
    mark: Vec<bool>,
}
