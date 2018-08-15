use std::{mem, ops};

type Free = Option<usize>;

pub struct Store<T> {
    values: Vec<Cell<T>>,
    free: Free,
    len: usize,
}

impl<T> Store<T> {
    pub fn new() -> Self {
        Store {
            values: Vec::new(),
            free: None,
            len: 0,
        }
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

    pub fn get(&self, index: usize) -> Option<&T> {
        self.values[index].as_ref().value()
    }

    pub fn add(&mut self, value: T) -> usize {
        self.len += 1;
        match self.free {
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
        self.try_remove(index).expect("freeing free cell")
    }

    pub fn try_remove(&mut self, index: usize) -> Option<T> {
        if let cell @ Cell::Value(_) = &mut self.values[index] {
            let value = mem::replace(cell, Cell::Free(self.free))
                .value()
                .expect("cell changed");
            self.free = Some(index);
            self.len -= 1;
            Some(value)
        } else {
            None
        }
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
        match &self.values[index] {
            Cell::Value(value) => value,
            Cell::Free(_) => panic!("freed cell"),
        }
    }
}

impl<T> ops::IndexMut<usize> for Store<T> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        match &mut self.values[index] {
            Cell::Value(value) => value,
            Cell::Free(_) => panic!("freed cell"),
        }
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
