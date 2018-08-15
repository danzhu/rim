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
}

impl Value for () {}

impl Value for String {}

#[derive(Clone, Debug)]
pub enum Error {
    NonCallable(Ref),
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Copy, Debug)]
pub struct Ref {
    index: usize,
}

#[derive(Clone, Debug, Default)]
pub struct Scope {
    binds: HashMap<String, Ref>,
}

impl Scope {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, name: &str) -> Option<Ref> {
        self.binds.get(name).cloned()
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

impl Value for Seq {}

#[derive(Clone, Debug)]
pub struct Func {
    param: String,
    body: ast::Expr,
    env: Scope,
}

impl Value for Func {
    fn as_any(&self) -> &any::Any {
        self
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

impl Value for Native {
    fn as_any(&self) -> &any::Any {
        self
    }
}

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

    pub fn free(&mut self, r: Ref) {
        self.free.push(r);
        let cell = self.cell_mut(r);
        assert!(cell.is_some(), "double free");
        *cell = None;
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
                let name = bind.names.last().expect("empty bind");
                Ok(env.binds[name])
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
