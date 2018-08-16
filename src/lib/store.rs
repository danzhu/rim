use std::{mem, ops};

type Free = Option<usize>;

pub struct Store<T> {
    values: Vec<Cell<T>>,
    free: Free,
    len: usize,
    reuse: bool,
}

impl<T> Store<T> {
    pub fn new() -> Self {
        Store {
            values: Vec::new(),
            free: None,
            len: 0,
            reuse: true,
        }
    }

    pub fn reuse(&mut self, reuse: bool) {
        self.reuse = reuse;
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn capacity(&self) -> usize {
        self.values.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, &T)> {
        self.values
            .iter()
            .enumerate()
            .filter_map(|(i, c)| c.as_ref().value().map(|v| (i, v)))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (usize, &mut T)> {
        self.values
            .iter_mut()
            .enumerate()
            .filter_map(|(i, c)| c.as_mut().value().map(|v| (i, v)))
    }

    pub fn keys<'a>(&'a self) -> impl Iterator<Item = usize> + 'a {
        self.values
            .iter()
            .enumerate()
            .filter_map(|(i, c)| c.as_ref().value().map(|_| i))
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.values.get(index).and_then(|c| c.as_ref().value())
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.values.get_mut(index).and_then(|c| c.as_mut().value())
    }

    pub fn add(&mut self, value: T) -> usize {
        self.len += 1;
        let free = if self.reuse { self.free } else { None };
        match free {
            Some(index) => {
                let cell = &mut self.values[index];
                self.free = cell.free().expect("free list pointing to non-free cell");
                *cell = Cell::Value(value);
                index
            }
            None => {
                let index = self.values.len();
                self.values.push(Cell::Value(value));
                index
            }
        }
    }

    pub fn remove(&mut self, index: usize) -> T {
        let cell = mem::replace(&mut self.values[index], Cell::Free(self.free));
        self.free = Some(index);
        self.len -= 1;
        cell.value().expect("removing free cell")
    }
}

impl<T> Default for Store<T> {
    fn default() -> Self {
        Store::new()
    }
}

impl<T> ops::Index<usize> for Store<T> {
    type Output = T;

    fn index(&self, index: usize) -> &T {
        self.get(index).expect("index out of range or removed")
    }
}

impl<T> ops::IndexMut<usize> for Store<T> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        self.get_mut(index).expect("index out of range or removed")
    }
}

enum Cell<T> {
    Value(T),
    Free(Free),
}

impl<T> Cell<T> {
    fn as_ref(&self) -> Cell<&T> {
        match self {
            Cell::Value(value) => Cell::Value(value),
            Cell::Free(free) => Cell::Free(*free),
        }
    }

    fn as_mut(&mut self) -> Cell<&mut T> {
        match self {
            Cell::Value(value) => Cell::Value(value),
            Cell::Free(free) => Cell::Free(*free),
        }
    }

    fn value(self) -> Option<T> {
        match self {
            Cell::Value(value) => Some(value),
            Cell::Free(_) => None,
        }
    }

    fn free(&self) -> Option<Free> {
        match self {
            Cell::Value(_) => None,
            Cell::Free(free) => Some(*free),
        }
    }

    fn has_value(&self) -> bool {
        match self {
            Cell::Value(_) => true,
            Cell::Free(_) => false,
        }
    }
}
