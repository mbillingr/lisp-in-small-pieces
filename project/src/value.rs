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

    pub fn is_pair(&self) -> bool {
        self.as_pair().is_some()
    }

    pub fn as_pair(&self) -> Option<&Rc<(Value, Value)>> {
        match self {
            Value::Pair(p) => Some(p),
            _ => None,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Value::*;
        match self {
            Undefined => Ok(()),
            Uninitialized => write!(f, "*uninitialized*"),
            Nil => write!(f, "'()"),
            Int(i) => write!(f, "{}", i),
            Pair(pair) => {
                let mut pair = pair;
                write!(f, "(")?;
                write!(f, "{}", pair.0)?;
                while let Pair(ref p) = pair.1 {
                    pair = p;
                    write!(f, " {}", pair.0)?;
                }
                if let Nil = pair.1 {
                    write!(f, ")")
                } else {
                    write!(f, " . {})", pair.1)
                }
            }
            Procedure(proc) => write!(f, "{}", proc),
            Box(x) => write!(f, "[{}]", x),
        }
    }
}
