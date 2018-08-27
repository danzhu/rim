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
    pub variants: Vec<Variant>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for variant in &self.variants {
            write!(f, "\n| {}", variant)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<String>,
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        for field in &self.fields {
            write!(f, " {}", field)?;
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
            ExprKind::Unit => write!(f, "()"),
            ExprKind::String(s) => write!(f, "{:?}", s),
            ExprKind::Bind(bind) => write!(f, "{}", bind),
            ExprKind::Apply { func, arg } => write!(f, "({} {})", func, arg),
            ExprKind::Func { param, body } => write!(f, "/{} {}", param, body),
            ExprKind::Seq { task, next } => write!(f, "({} > {})", task, next),
            ExprKind::Match { expr, arms } => {
                write!(f, "({}", expr)?;
                for arm in arms {
                    write!(f, " | {}", arm)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Unit,
    String(String),
    Bind(Bind),
    Apply { func: Box<Expr>, arg: Box<Expr> },
    Func { param: Pattern, body: Box<Expr> },
    Seq { task: Box<Expr>, next: Box<Expr> },
    Match { expr: Box<Expr>, arms: Vec<Arm> },
}

#[derive(Clone, Debug)]
pub struct Arm {
    pub pattern: Pattern,
    pub value: Expr,
}

impl fmt::Display for Arm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.pattern, self.value)
    }
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub kind: PatternKind,
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            PatternKind::Bind(name) => write!(f, "{}", name),
            PatternKind::Struct { tp, fields } => {
                write!(f, "({}", tp)?;
                for field in fields {
                    write!(f, " {}", field)?;
                }
                write!(f, ")")
            }
            PatternKind::Ignore => write!(f, "_"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum PatternKind {
    Bind(String),
    Struct { tp: Bind, fields: Vec<Pattern> },
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
