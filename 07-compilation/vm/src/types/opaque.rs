#[derive(Debug, Copy, Clone, PartialEq)]
pub struct OpaquePointer(usize);

impl OpaquePointer {
    pub fn new(addr: usize) -> Self {
        OpaquePointer(addr)
    }

    pub unsafe fn into<T: OpaqueCast>(self) -> T {
        T::from_op(self)
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }

    pub fn ptr_eq<T: OpaqueCast>(&self, other: &T) -> bool {
        self.0 == other.as_op().0
    }
}

pub trait OpaqueCast {
    unsafe fn from_op(op: OpaquePointer) -> Self;
    fn as_op(&self) -> OpaquePointer;
}

impl OpaqueCast for usize {
    unsafe fn from_op(op: OpaquePointer) -> Self {
        op.0
    }

    fn as_op(&self) -> OpaquePointer {
        OpaquePointer(*self)
    }
}
