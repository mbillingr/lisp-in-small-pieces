use crate::description::Arity;
use crate::scm::Scm;

#[derive(Copy, Clone)]
pub struct RuntimePrimitive {
    pub func: fn(args: &[Scm]) -> Scm,
    pub arity: Arity,
}

impl RuntimePrimitive {
    pub fn new(arity: Arity, func: fn(args: &[Scm]) -> Scm) -> Self {
        RuntimePrimitive { arity, func }
    }

    pub fn invoke(&self, args: &[Scm]) -> Scm {
        if self.arity.check(args.len()) {
            (self.func)(&args)
        } else {
            panic!("Incorrect arity")
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
