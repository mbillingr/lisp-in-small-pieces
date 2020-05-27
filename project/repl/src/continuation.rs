use crate::interpreter::{CallstackFrame, CallstackItem, VirtualMachine};
use crate::error::{Result, RuntimeError};
use crate::scm::Scm;

#[derive(Debug)]
pub struct Continuation {
    pub value_stack: Vec<Scm>,
    pub call_stack: Vec<CallstackItem>,
    pub exception_handler: Scm,
}

#[derive(Debug)]
pub struct ExitProcedure {
    state: CallstackFrame,
    value_stack_height: usize,
    call_stack_height: usize,
    exception_handler: Scm,
}

impl Continuation {
    pub fn new(ip_offset: isize, vm: &VirtualMachine) -> Self {
        let value_stack = vm.value_stack.clone();
        let mut call_stack = vm.call_stack.clone();
        call_stack.push(CallstackItem::Frame(
            vm.current_frame().with_ip_offset(ip_offset),
        ));

        Continuation {
            value_stack,
            call_stack,
            exception_handler: vm.exception_handler,
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
        vm.exception_handler = self.exception_handler;

        // call/cc pushed the place we need to go to before saving the stack
        vm.pop_state()
    }

    pub fn invoke_tail(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        self.invoke(nargs, vm)
    }
}

impl ExitProcedure {
    pub fn new(ip_offset: isize, vm: &VirtualMachine) -> Self {
        ExitProcedure {
            state: vm.current_frame().with_ip_offset(ip_offset),
            value_stack_height: vm.value_stack.len(),
            call_stack_height: vm.call_stack.len(),
            exception_handler: vm.exception_handler,
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

        match vm.call_stack.get(self.call_stack_height) {
            Some(CallstackItem::ExitProc(ep)) if std::ptr::eq(*ep, self) => {}
            _ => return Err(RuntimeError::InvalidExitProcedure.into()),
        }

        vm.value_stack.truncate(self.value_stack_height);
        vm.call_stack.truncate(self.call_stack_height);
        vm.exception_handler = self.exception_handler;
        vm.set_frame(self.state);

        Ok(())
    }

    pub fn invoke_tail(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        self.invoke(nargs, vm)
    }
}
