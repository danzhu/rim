#[derive(Clone, Debug)]
pub struct Decl {
    pub name: String,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Bind(Bind),
    Apply(Box<Expr>, Box<Expr>),
    Func(String, Box<Expr>),
    Seq(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct Bind {
    pub names: Vec<String>,
}
