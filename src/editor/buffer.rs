#[derive(Clone, Debug, Default)]
pub struct Buffer {
    lines: Vec<String>,
}

impl Buffer {
    pub fn new() -> Self {
        Default::default()
    }
}
