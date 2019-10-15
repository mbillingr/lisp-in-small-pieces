use super::{ActivationFrame, CodePointer};
use crate::vm::VirtualMachine;

#[derive(Debug, Copy, Clone)]
pub struct Closure {
    code: CodePointer,
    closed_environment: &'static ActivationFrame,
}

impl Closure {
    pub fn new(code: CodePointer, env: &'static ActivationFrame) -> Self {
        Closure {
            code,
            closed_environment: env,
        }
    }

    pub fn allocate(code: CodePointer, env: &'static ActivationFrame) -> &'static Self {
        Box::leak(Box::new(Self::new(code, env)))
    }

    pub fn invoke(&self, vm: &mut VirtualMachine) {
        vm.env = self.closed_environment;
        vm.pc = self.code;
    }

    pub fn code(&self) -> &CodePointer {
        &self.code
    }

    pub fn env(&self) -> &ActivationFrame {
        self.closed_environment
    }
}
