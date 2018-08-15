mod buffer;

use self::buffer::Buffer;
use lib::Store;

#[derive(Default)]
pub struct Editor {
    buffers: Store<Buffer>,
}

impl Editor {
    pub fn new() -> Self {
        Default::default()
    }
}
