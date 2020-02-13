use crate::bytecode::{CallstackFrame, VirtualMachine};
use crate::error::Result;
use crate::scm::Scm;

#[derive(Debug)]
pub struct Continuation {
    pub value_stack: Vec<Scm>,
    pub call_stack: Vec<CallstackFrame>,
}

impl Continuation {
    pub fn new(value_stack: Vec<Scm>, call_stack: Vec<CallstackFrame>) -> Self {
        Continuation {
            value_stack,
            call_stack,
        }
    }

    pub fn invoke(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        match nargs {
            0 => vm.val = Scm::Undefined,
            1 => vm.val = vm.pop_value()?,
            _ => unimplemented!("more than 1 closure arg"),
        }

        vm.value_stack = self.value_stack.clone();
        vm.call_stack = self.call_stack.clone();

        // call/cc pushed the place we need to go to before saving the stack
        vm.pop_state()
    }

    pub fn invoke_tail(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        self.invoke(nargs, vm)
    }
}
