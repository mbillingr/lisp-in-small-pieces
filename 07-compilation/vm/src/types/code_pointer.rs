use super::{OpaqueCast, OpaquePointer};
use crate::Op;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CodePointer {
    ptr: *const u8,
}

impl CodePointer {
    pub fn new(code: &Op) -> Self {
        CodePointer {
            ptr: code as *const _ as *const u8,
        }
    }

    pub unsafe fn new_unchecked(code: &u8) -> Self {
        CodePointer { ptr: code }
    }

    pub unsafe fn fetch_instruction(&mut self) -> Op {
        // self.pc must always point to a valid code location
        Op::from_u8_unchecked(self.fetch_byte())
    }

    pub unsafe fn fetch_byte(&mut self) -> u8 {
        // self.pc must always point to a valid code location
        let b = *self.ptr;
        self.ptr = self.ptr.offset(1);
        b
    }

    /*unsafe fn peek_instruction(&self) -> Op {
        // self.pc must always point to a valid code location
        Op::from_u8_unchecked(self.peek_byte())
    }

    unsafe fn peek_byte(&self) -> u8 {
        // self.pc must always point to a valid code location
        *self.ptr
    }*/

    pub unsafe fn offset(&self, offset: isize) -> Self {
        CodePointer {
            ptr: self.ptr.offset(offset),
        }
    }
}

impl OpaqueCast for CodePointer {
    unsafe fn from_op(op: OpaquePointer) -> Self {
        CodePointer {
            ptr: op.as_usize() as _,
        }
    }

    fn as_op(&self) -> OpaquePointer {
        OpaquePointer::new(self.ptr as usize)
    }
}

impl OpaqueCast for &Op {
    unsafe fn from_op(_op: OpaquePointer) -> Self {
        unimplemented!()
    }

    fn as_op(&self) -> OpaquePointer {
        OpaquePointer::new(CodePointer::new(self).ptr as usize)
    }
}

impl OpaqueCast for &[Op] {
    unsafe fn from_op(_op: OpaquePointer) -> Self {
        unimplemented!()
    }

    fn as_op(&self) -> OpaquePointer {
        OpaquePointer::new(CodePointer::new(&self[0]).ptr as usize)
    }
}

/*impl From<Value> for CodePointer {
    fn from(v: Value) -> Self {
        CodePointer {
            ptr: v.as_pointer().unwrap(),
        }
    }
}*/
