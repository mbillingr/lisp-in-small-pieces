use crate::ast::RuntimeProcedure;
use crate::sexpr::{self, Sexpr, TrackedSexpr};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Undefined,
    Uninitialized,
    Nil,
    Int(i64),

    Pair(Rc<(Value, Value)>),

    Procedure(RuntimeProcedure),

    Box(Box<Value>),
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl From<Sexpr> for Value {
    fn from(e: Sexpr) -> Self {
        match e {
            Sexpr::Nil => Value::Nil,
            Sexpr::Int(i) => Value::Int(i),
            _ => unimplemented!("{:?}", e),
        }
    }
}

impl From<TrackedSexpr> for Value {
    fn from(e: TrackedSexpr) -> Self {
        match e.into_sexpr() {
            Sexpr::Nil => Value::Nil,
            Sexpr::Int(i) => Value::Int(i),
            e => unimplemented!("{:?}", e),
        }
    }
}

impl Value {
    pub fn nil() -> Self {
        Value::Nil
    }

    pub fn cons(a: Value, b: Value) -> Self {
        Value::Pair(Rc::new((a, b)))
    }
}
