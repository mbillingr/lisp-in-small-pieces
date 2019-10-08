use lisp_core::lexpr;
use crate::{Closure, ActivationFrame, OpaqueCast, OpaquePointer, CodePointer};


//pub const DYNENV_TAG: Value = Value::Symbol("*dynenv*");
pub const DYNENV_TAG: Scm = Scm {ptr: 123};


#[derive(Debug, Copy, Clone)]
pub struct Scm {
    ptr: usize,
}


#[derive(Debug, Copy, Clone)]
pub enum Value {
    Null,
    Uninitialized,
    Int(i64),
    Pair(Scm, Scm),
    Symbol(&'static str),
    String(&'static String),

    Frame(&'static ActivationFrame),
    Closure(&'static Closure),
    Pointer(&'static u8),
}

impl Scm {
    pub fn from_static_value(value: &'static Value) -> Self {
        Scm {
            ptr: value as *const _ as usize
        }
    }

    pub fn from_value(value: Value) -> Self {
        Scm {
            ptr: Box::leak(Box::new(value)) as *const _ as usize
        }
    }

    pub fn null() -> Self {
        Self::from_static_value(&Value::Null)
    }

    pub fn uninitialized() -> Self {
        Self::from_static_value(&Value::Uninitialized)
    }

    pub fn int(i: i64) -> Self {
        Self::from_value(Value::Int(i))
    }

    pub fn cons(car: Scm, cdr: Scm) -> Self {
        Self::from_value(Value::Pair(car, cdr))
    }

    pub fn symbol(s: &str) -> Self {
        let s = Box::leak(Box::new(s.to_owned()));
        Scm::from_value(Value::Symbol(s))
    }

    pub fn string(s: &str) -> Self {
        let s = Box::leak(Box::new(s.to_owned()));
        Scm::from_value(Value::String(s))
    }

    pub fn frame(size: usize) -> Self {
        let frm = ActivationFrame::allocate(size);
        Scm::from_value(Value::Frame(frm))
    }

    pub fn closure(code: CodePointer, env: &'static ActivationFrame) -> Self {
        let cls = Closure::allocate(code, env);
        Self::from_value(Value::Closure(cls))
    }

    fn as_ptr(&self) -> *const Value {
        self.ptr as *const Value
    }

    unsafe fn deref(&self) -> &Value {
        unsafe {
            &*self.as_ptr()
        }
    }

    pub fn is_uninitialized(&self) -> bool {
        unsafe {
            (*self.as_ptr()).is_uninitialized()
        }
    }

    pub fn is_closure(&self) -> bool {
        self.as_closure().is_some()
    }

    pub fn as_frame(&self) -> Option<&'static ActivationFrame> {
        unsafe {
            self.deref().as_frame()
        }
    }

    pub fn as_closure(&self) -> Option<&'static Closure> {
        unsafe {
            self.deref().as_closure()
        }
    }

    pub fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }

    pub fn eqv(&self, other: &Self) -> bool {
        unsafe {
            self.deref() == other.deref()
        }
    }
}

impl OpaqueCast for Scm {
    unsafe fn from_op(op: OpaquePointer) -> Self {
        Scm {
            ptr: op.0
        }
    }

    fn as_op(&self) -> OpaquePointer {
        OpaquePointer(self.ptr)
    }
}

impl Value {
    pub fn is_uninitialized(&self) -> bool {
        match self {
            Value::Uninitialized => true,
            _ => false,
        }
    }

    pub fn as_usize(&self) -> Option<usize> {
        match self {
            Value::Int(i) => Some(*i as usize),
            _ => None,
        }
    }

    pub fn as_frame(&self) -> Option<&'static ActivationFrame> {
        match self {
            Value::Frame(frame) => Some(frame),
            _ => None,
        }
    }

    pub fn as_closure(&self) -> Option<&'static Closure> {
        match self {
            Value::Closure(cls) => Some(cls),
            _ => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Null, Null) => true,
            (Int(a), Int(b)) => a == b,
            (Pair(aa, ad), Pair(ba, bd)) => aa.eq(ba) && ad.eq(bd),
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            _ => false,
        }
    }
}

impl From<&lexpr::Value> for Scm {
    fn from(exp: &lexpr::Value) -> Self {
        use lexpr::Value::*;
        match exp {
            Null => Scm::null(),
            Number(n) if n.is_i64() => Scm::int(n.as_i64().unwrap()),
            Cons(pair) => Scm::cons(pair.car().into(), pair.cdr().into()),
            Symbol(s) => Scm::symbol(s),
            String(s) => Scm::string(s),
            _ => unimplemented!(),
        }
    }
}
