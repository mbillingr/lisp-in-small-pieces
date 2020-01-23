use crate::error::{Error, RuntimeError};
use crate::scm::Scm;

pub type Result = std::result::Result<Scm, Error>;

pub type PrimitiveSignature = fn(args: &[Scm]) -> Result;

#[derive(Copy, Clone)]
pub struct RuntimePrimitive {
    pub name: &'static str,
    func: PrimitiveSignature,
    arity: Arity,
}

impl RuntimePrimitive {
    pub fn new(name: &'static str, arity: Arity, func: PrimitiveSignature) -> Self {
        RuntimePrimitive { name, arity, func }
    }

    pub fn name(&self) -> &str {
        self.name
    }

    pub fn func(&self) -> &PrimitiveSignature {
        &self.func
    }

    pub fn arity(&self) -> Arity {
        self.arity
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
        write!(
            f,
            "{}@{:p}",
            self.name, self.func as *const PrimitiveSignature
        )
    }
}

impl std::fmt::Display for RuntimePrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}@{:p}",
            self.name, self.func as *const PrimitiveSignature
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Arity {
    Exact(u16),
    AtLeast(u16),
}

impl Arity {
    pub fn check(&self, n_args: usize) -> bool {
        match *self {
            Arity::Exact(n) => n_args == n as usize,
            Arity::AtLeast(n) => n_args >= n as usize,
        }
    }
}
