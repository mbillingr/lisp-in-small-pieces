use crate::sexpr::Sexpr;
use crate::ast::RuntimeProcedure;

pub type Symbol = &'static str;

#[derive(Debug, Clone)]
pub enum Value {
    Undefined,
    Int(i64),
    Procedure(RuntimeProcedure),
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl Value {
    pub fn cons(a: Value, b: Value) -> Self {
        unimplemented!()
    }
}