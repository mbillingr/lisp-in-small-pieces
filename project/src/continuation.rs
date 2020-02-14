use crate::bytecode::{CallstackFrame, CallstackItem, VirtualMachine};
use crate::error::{Result, RuntimeError};
use crate::scm::Scm;

#[derive(Debug)]
pub struct Continuation {
    pub value_stack: Vec<Scm>,
    pub call_stack: Vec<CallstackItem>,
}

#[derive(Debug)]
pub struct ExitProcedure {
    state: CallstackFrame,
    value_stack_height: usize,
    call_stack_height: usize,
}

impl Continuation {
    pub fn new(vm: &VirtualMachine) -> Self {
        let value_stack = vm.value_stack.clone();
        let mut call_stack = vm.call_stack.clone();
        call_stack.push(CallstackItem::Frame(vm.current_frame()));

        Continuation {
            value_stack,
            call_stack,
        }
    }

    pub fn invoke(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        match nargs {
            0 => vm.val = Scm::Undefined,
            1 => vm.val = vm.pop_value()?,
            _ => unimplemented!("more than 1 continuation arg"),
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

impl ExitProcedure {
    pub fn new(vm: &VirtualMachine) -> Self {
        ExitProcedure {
            state: vm.current_frame(),
            value_stack_height: vm.value_stack.len(),
            call_stack_height: vm.call_stack.len(),
        }
    }

    pub fn invoke(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        match nargs {
            0 => vm.val = Scm::Undefined,
            1 => vm.val = vm.pop_value()?,
            _ => unimplemented!("more than 1 continuation arg"),
        }

        // todo: Could it be possible to check at compile time if the exit procedure escapes the valid scope?
        //          1. Probably not, because the ep might be passet to an unknown function
        //          2. Probably not due to the presence of call/cc

        // TODO: make call/ep play along nicely with dynamic-wind

        match vm.call_stack.get(self.call_stack_height) {
            Some(CallstackItem::ExitProc(ep)) if std::ptr::eq(*ep, self) => {}
            _ => return Err(RuntimeError::InvalidExitProcedure.into()),
        }

        vm.value_stack.truncate(self.value_stack_height);
        vm.call_stack.truncate(self.call_stack_height);
        vm.set_frame(self.state);

        Ok(())
    }

    pub fn invoke_tail(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        self.invoke(nargs, vm)
    }
}
