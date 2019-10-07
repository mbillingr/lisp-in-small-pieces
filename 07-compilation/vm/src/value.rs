use crate::{ActivationFrame, Closure};
use lisp_core::lexpr;

pub const DYNENV_TAG: Value = Value::Symbol("*dynenv*");

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Null,
    Uninitialized,
    True,
    False,
    Int(i64),
    Pair(&'static [Value; 2]),
    Symbol(&'static str),
    String(&'static String),

    Frame(&'static ActivationFrame),
    Closure(&'static Closure),
    Pointer(*const u8),
}

impl Value {
    pub fn null() -> Self {
        Value::Null
    }

    pub fn uninitialized() -> Self {
        Value::Uninitialized
    }

    pub fn bool(b: bool) -> Self {
        match b {
            true => Value::True,
            false => Value::False,
        }
    }

    pub fn int(i: i64) -> Self {
        Value::Int(i)
    }

    pub fn cons(car: Value, cdr: Value) -> Self {
        Value::Pair(Box::leak(Box::new([car, cdr])))
    }

    pub fn symbol(s: &str) -> Self {
        Value::Symbol(Box::leak(Box::new(s.to_owned())))
    }

    pub fn string(s: &str) -> Self {
        Value::String(Box::leak(Box::new(s.to_owned())))
    }

    pub fn is_false(&self) -> bool {
        match self {
            Value::False => true,
            _ => false,
        }
    }

    pub fn is_uninitialized(&self) -> bool {
        match self {
            Value::Uninitialized => true,
            _ => false,
        }
    }

    pub fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }

    pub fn eqv(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Null, Null) => true,
            (Uninitialized, _) => false,
            (_, Uninitialized) => false,
            (Int(a), Int(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            _ => self.eq(other),
        }
    }

    pub fn as_usize(&self) -> Option<usize> {
        match self {
            Value::Int(i) => Some(*i as usize),
            _ => None,
        }
    }

    pub fn as_pointer(&self) -> Option<*const u8> {
        match self {
            Value::Pointer(p) => Some(*p),
            _ => None,
        }
    }

    pub fn as_frame(&self) -> Option<&'static ActivationFrame> {
        match self {
            Value::Frame(frame) => Some(frame),
            _ => None,
        }
    }

    pub fn less(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::bool(a < b),
            _ => panic!("Type Error"),
        }
    }

    pub fn add(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            _ => panic!("Type Error"),
        }
    }

    pub fn sub(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
            _ => panic!("Type Error"),
        }
    }

    pub fn mul(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            _ => panic!("Type Error"),
        }
    }
}

impl From<&lexpr::Value> for Value {
    fn from(exp: &lexpr::Value) -> Self {
        use lexpr::Value::*;
        match exp {
            Null => Value::null(),
            Number(n) if n.is_i64() => Value::int(n.as_i64().unwrap()),
            Cons(pair) => Value::cons(pair.car().into(), pair.cdr().into()),
            Symbol(s) => Value::symbol(s),
            String(s) => Value::string(s),
            _ => unimplemented!(),
        }
    }
}
