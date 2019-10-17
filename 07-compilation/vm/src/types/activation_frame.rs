use crate::types::{OpaqueCast, OpaquePointer, Scm};
use std::cell::Cell;

#[derive(Debug, Default, PartialEq)]
pub struct ActivationFrame {
    next: Cell<Option<&'static ActivationFrame>>,
    slots: Vec<Scm>,
}

impl ActivationFrame {
    pub fn new(size: usize) -> Self {
        ActivationFrame {
            next: Cell::new(None),
            slots: vec![Scm::uninitialized(); size],
        }
    }

    pub fn allocate(size: usize) -> &'static Self {
        Box::leak(Box::new(Self::new(size)))
    }

    pub fn len(&self) -> usize {
        self.slots.len()
    }

    pub fn next(&self) -> Option<&'static ActivationFrame> {
        self.next.get()
    }

    pub fn argument(&self, idx: usize) -> &Scm {
        &self.slots[idx]
    }

    pub fn set_argument(&self, idx: usize, value: Scm) {
        unsafe {
            *(&self.slots[idx] as *const _ as *mut _) = value;
        }
    }

    pub fn extends(&self, env: &'static ActivationFrame) -> &Self {
        self.next.set(Some(env));
        self
    }

    pub fn deep_fetch(&self, i: usize, j: usize) -> &Scm {
        let mut env = self;
        for _ in 0..i {
            env = env.next().unwrap();
        }
        env.argument(j)
    }

    pub fn deep_set(&self, i: usize, j: usize, value: Scm) {
        unsafe {
            *(self.deep_fetch(i, j) as *const _ as *mut _) = value;
        }
    }

    /// Put excess arguments as a list after the last argument
    pub fn listify(&self, arity: usize) {
        let mut list = Scm::null();
        for x in self.slots[arity..self.len() - 1].iter().rev().copied() {
            list = Scm::cons(x, list);
        }
        self.set_argument(arity, list);
    }
}

impl OpaqueCast for &'static ActivationFrame {
    unsafe fn from_op(op: OpaquePointer) -> Self {
        &*(op.as_usize() as *const _)
    }

    fn as_op(&self) -> OpaquePointer {
        OpaquePointer::new(*self as *const _ as _)
    }
}
