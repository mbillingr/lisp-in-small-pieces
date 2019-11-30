use crate::symbol::Symbol;
use std::cell::Cell;

#[derive(Debug, Copy, Clone)]
pub enum Scm {
    Undefined,
    Uninitialized,
    Nil,
    True,
    False,
    Int(i64),
    Float(f64),
    Symbol(Symbol),
    String(&'static str),
    Vector(&'static [Scm]),

    Pair(&'static (Scm, Scm)),

    /*Procedure(RuntimeProcedure),
    Primitive(RuntimePrimitive),*/
    Cell(&'static Cell<Scm>),
}
