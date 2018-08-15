use super::ast;
use lib::Store;
use std::collections::HashMap;
use std::rc::Rc;
use std::{any, fmt, mem, result};

pub trait AsAny: 'static {
    fn as_any_imp(&self) -> &any::Any;

    fn as_any_mut_imp(&mut self) -> &mut any::Any;
}

impl<T: any::Any> AsAny for T {
    fn as_any_imp(&self) -> &any::Any {
        self
    }

    fn as_any_mut_imp(&mut self) -> &mut any::Any {
        self
    }
}

pub trait Value: AsAny + fmt::Debug {
    fn as_any(&self) -> &any::Any {
        self.as_any_imp()
    }

    fn as_any_mut(&mut self) -> &mut any::Any {
        self.as_any_mut_imp()
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
    env: Ref,
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
    func: Rc<Fn(&mut Memory, Ref) -> Result<Ref>>,
}

impl Native {
    pub fn new<F>(f: F) -> Self
    where
        F: Fn(&mut Memory, Ref) -> Result<Ref> + 'static,
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
pub struct Memory {
    store: Store<Box<Value>>,
    debug: bool,
}

impl Memory {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn debug(&mut self, debug: bool) {
        self.debug = debug;
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
                self.store.try_remove(i);
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

    pub fn fetch(&self, name: &str, scope: &Scope) -> Result<Ref> {
        if let Some(r) = scope.binds.get(name) {
            Ok(*r)
        } else if let Some(p) = scope.parent {
            let p = self.get::<Scope>(p).expect("scope parent not scope");
            self.fetch(name, p)
        } else {
            Err(Error::NoMember(scope.clone(), name.to_string()))
        }
    }

    pub fn insert<Str, T>(&mut self, name: Str, val: T, env: &mut Scope)
    where
        Str: Into<String>,
        T: Value,
    {
        env.binds.insert(name.into(), self.alloc(val));
    }

    pub fn load(&mut self, decls: Vec<ast::Decl>, env: Ref) -> Result<()> {
        for decl in decls {
            let r = self.eval(&decl.value, env)?;

            let mut scope = self.get_mut::<Scope>(env).expect("load env not scope");
            scope.binds.insert(decl.name, r);
        }
        Ok(())
    }

    pub fn call(&mut self, func: Ref, arg: Ref) -> Result<Ref> {
        if let Some(native) = self.get::<Native>(func).cloned() {
            (native.func)(self, arg)
        } else if let Some(func) = self.get::<Func>(func).cloned() {
            let mut scope = Scope::new();
            scope.parent = Some(func.env);
            scope.binds.insert(func.param, arg);
            let env = self.alloc(scope);

            self.eval(&func.body, env)
        } else {
            Err(Error::NonCallable(func))
        }
    }

    pub fn eval(&mut self, expr: &ast::Expr, env: Ref) -> Result<Ref> {
        match &expr.kind {
            ast::ExprKind::Bind(bind) => {
                let mut r = env;
                for name in &bind.names {
                    let scope = self.get::<Scope>(r).ok_or_else(|| Error::NonScope(r))?;
                    r = self.fetch(&name, scope)?
                }
                Ok(r)
            }
            ast::ExprKind::Apply(func, arg) => {
                let func = self.eval(&func, env)?;
                let arg = self.eval(&arg, env)?;
                self.call(func, arg)
            }
            ast::ExprKind::Func(name, body) => Ok(self.alloc(Func {
                param: name.clone(),
                body: (**body).clone(),
                env,
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
    memory: &'a Memory,
    result: GcResult,
}

impl<'a> Gc<'a> {
    pub fn new(memory: &'a Memory) -> Self {
        let mut mark = Vec::new();
        mark.resize(memory.store.capacity(), false);
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
