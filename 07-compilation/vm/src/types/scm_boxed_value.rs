use crate::types::{ActivationFrame, Closure, Escape, Primitive, Symbol};

#[derive(Copy, Clone)]
pub enum ScmBoxedValue {
    Symbol(Symbol),
    String(&'static String),

    Primitive(&'static Primitive),
    Frame(&'static ActivationFrame),
    Closure(&'static Closure),
    Escape(&'static Escape),
}

impl ScmBoxedValue {
    pub fn as_symbol(&self) -> Option<Symbol> {
        match self {
            ScmBoxedValue::Symbol(s) => Some(s),
            _ => None,
        }
    }
    pub fn as_string(&self) -> Option<&'static String> {
        match self {
            ScmBoxedValue::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_primitive(&self) -> Option<&'static Primitive> {
        match self {
            ScmBoxedValue::Primitive(p) => Some(p),
            _ => None,
        }
    }

    pub fn as_frame(&self) -> Option<&'static ActivationFrame> {
        match self {
            ScmBoxedValue::Frame(frame) => Some(frame),
            _ => None,
        }
    }

    pub fn as_closure(&self) -> Option<&'static Closure> {
        match self {
            ScmBoxedValue::Closure(cls) => Some(cls),
            _ => None,
        }
    }

    pub fn as_escape(&self) -> Option<&'static Escape> {
        match self {
            ScmBoxedValue::Escape(esc) => Some(esc),
            _ => None,
        }
    }
}

impl PartialEq for ScmBoxedValue {
    fn eq(&self, rhs: &Self) -> bool {
        use ScmBoxedValue::*;
        match (self, rhs) {
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            _ => false,
        }
    }
}
