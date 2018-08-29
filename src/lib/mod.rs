mod store;

pub use self::store::Store;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
}

impl Pos {
    pub fn new() -> Self {
        Pos { line: 1, column: 1 }
    }
}

impl Default for Pos {
    fn default() -> Self {
        Pos::new()
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Size {
    pub height: i32,
    pub width: i32,
}

impl Size {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Rect {
    pub pos: Pos,
    pub size: Size,
}

impl Rect {
    pub fn new() -> Self {
        Default::default()
    }
}
