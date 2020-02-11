use crate::bytecode::{Closure, VirtualMachine};
use crate::scm::Scm;

#[derive(Debug)]
pub struct Continuation {
    pub value_stack: Vec<Scm>,
    pub call_stack: Vec<(usize, isize, &'static Closure)>,
}

impl Continuation {
    pub fn new(value_stack: Vec<Scm>, call_stack: Vec<(usize, isize, &'static Closure)>) -> Self {
        Continuation {
            value_stack,
            call_stack,
        }
    }

    pub fn invoke(&self, nargs: usize, vm: &mut VirtualMachine) {
        let n = vm.value_stack.len() - nargs;
        let args = vm.value_stack[n..].to_vec();

        vm.value_stack = self.value_stack.clone();
        vm.call_stack = self.call_stack.clone();

        vm.value_stack.extend(args);

        // call/cc pushed the place we need to go to before saving the stack
        vm.pop_state();
    }
}
