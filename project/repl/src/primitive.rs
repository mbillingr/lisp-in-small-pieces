use crate::interpreter::VirtualMachine;
use crate::error::{Result, RuntimeError};
use crate::scm::Scm;
use sunny_common::Arity;

pub type PrimitiveSignature = fn(args: &[Scm], context: &mut VirtualMachine) -> Result<Scm>;

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

    pub fn invoke(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        let n = vm.value_stack.len() - nargs;
        let args = vm.value_stack[n..].to_vec();
        vm.value_stack.truncate(n);

        if self.arity.check(nargs) {
            vm.val = (self.func)(&args, vm)?;
            Ok(())
        } else {
            Err(RuntimeError::IncorrectArity.into())
        }
    }

    pub fn invoke_tail(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        self.invoke(nargs, vm)?;
        vm.do_return()
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

impl PartialEq for RuntimePrimitive {
    fn eq(&self, other: &Self) -> bool {
        self.func as *const PrimitiveSignature == other.func as *const PrimitiveSignature
    }
}
