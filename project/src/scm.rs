use crate::bytecode::CodeObject;
use crate::sexpr::{Sexpr, TrackedSexpr};
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
    Vector(&'static [Cell<Scm>]),

    Pair(&'static (Cell<Scm>, Cell<Scm>)),

    Closure(&'static CodeObject, &'static [Scm]),

    /*Procedure(RuntimeProcedure),
    Primitive(RuntimePrimitive),*/
    Cell(&'static Cell<Scm>),
}

impl Scm {
    pub fn uninitialized() -> Self {
        Scm::Uninitialized
    }

    pub fn closure(func: &'static CodeObject, free_vars: impl Into<Box<[Scm]>>) -> Self {
        Scm::Closure(func, Box::leak(free_vars.into()))
    }

    pub fn equals(&self, other: &Self) -> bool {
        use Scm::*;
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Vector(a), Vector(b)) => a.iter().zip(*b).all(|(a, b)| a.get().equals(&b.get())),
            (Pair(a), Pair(b)) => a.0.get().equals(&b.0.get()) && a.1.get().equals(&b.1.get()),
            (Cell(a), Cell(b)) => a.get().equals(&b.get()),
            _ => false,
        }
    }

    pub fn is_false(&self) -> bool {
        match self {
            Scm::False => true,
            _ => false,
        }
    }
}

impl From<Sexpr> for Scm {
    fn from(e: Sexpr) -> Self {
        match e {
            Sexpr::Nil => Scm::Nil,
            Sexpr::True => Scm::True,
            Sexpr::False => Scm::False,
            Sexpr::Int(i) => Scm::Int(i),
            Sexpr::Float(f) => Scm::Float(f),
            Sexpr::Symbol(s) => Scm::Symbol(s),
            /*Sexpr::String(s) => Scm::String(s),
            Sexpr::List(l, dot) => {
                let mut x = dot.map(|d| (*d).into()).unwrap_or(Scm::Nil);
                for item in l.iter().rev() {
                    x = Scm::cons(item.clone().into(), x);
                }
                x
            }
            Sexpr::Vector(v) => {
                let items: Vec<Scm> = v.iter().map(|i| i.clone().into()).collect();
                Scm::Vector(items.into())
            }*/
            _ => unimplemented!(),
        }
    }
}

impl From<TrackedSexpr> for Scm {
    fn from(e: TrackedSexpr) -> Self {
        e.into_sexpr().into()
    }
}
