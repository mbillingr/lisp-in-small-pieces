use crate::description::Arity;
use crate::error::{Error, RuntimeError};
use crate::scm::Scm;

pub type Result = std::result::Result<Scm, Error>;

pub type PrimitiveSignature = fn(args: &[Scm]) -> Result;

#[derive(Copy, Clone)]
pub struct RuntimePrimitive {
    pub name: &'static str,
    pub func: PrimitiveSignature,
    pub arity: Arity,
}

impl RuntimePrimitive {
    pub fn new(name: &'static str, arity: Arity, func: PrimitiveSignature) -> Self {
        RuntimePrimitive { name, arity, func }
    }

    pub fn invoke(&self, args: &[Scm]) -> Result {
        if self.arity.check(args.len()) {
            (self.func)(&args)
        } else {
            Err(RuntimeError::IncorrectArity.into())
        }
    }
}

impl std::fmt::Debug for RuntimePrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "*primitive*")
    }
}

impl std::fmt::Display for RuntimePrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "*primitive*")
    }
}
