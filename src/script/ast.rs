use std::fmt;

#[derive(Clone, Debug)]
pub struct Decl {
    pub name: String,
    pub kind: DeclKind,
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.name, self.kind)
    }
}

#[derive(Clone, Debug)]
pub enum DeclKind {
    Type(Type),
    Bind(Expr),
}

impl fmt::Display for DeclKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DeclKind::Type(tp) => write!(f, "{}", tp),
            DeclKind::Bind(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Type {
    pub fields: Vec<String>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for field in &self.fields {
            write!(f, "{}", field)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ExprKind::Bind(bind) => write!(f, "{}", bind),
            ExprKind::Apply(func, arg) => write!(f, "({} {})", func, arg),
            ExprKind::Func(param, body) => write!(f, "/{} {}", param, body),
            ExprKind::Seq(task, next) => write!(f, "({} > {})", task, next),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Bind(Bind),
    Apply(Box<Expr>, Box<Expr>),
    Func(Pattern, Box<Expr>),
    Seq(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub kind: PatternKind,
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            PatternKind::Bind(name) => write!(f, "{}", name),
            PatternKind::Ignore => write!(f, "_"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum PatternKind {
    Bind(String),
    Ignore,
}

#[derive(Clone, Debug)]
pub struct Bind {
    pub names: Vec<String>,
}

impl fmt::Display for Bind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.names[0])?;
        for name in self.names[1..].iter() {
            write!(f, ":{}", name)?;
        }
        Ok(())
    }
}
