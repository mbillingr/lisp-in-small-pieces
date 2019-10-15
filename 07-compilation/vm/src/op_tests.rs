use crate::{
    types::{ActivationFrame, CodePointer, Scm},
    vm::VirtualMachine,
    Op,
};

fn init_machine() -> VirtualMachine {
    VirtualMachine {
        val: Scm::uninitialized(),
        fun: Scm::uninitialized(),
        arg1: Scm::uninitialized(),
        arg2: Scm::uninitialized(),
        env: ActivationFrame::allocate(0),
        constants: vec![],
        mut_globals: vec![],
        globals: vec![],
        stack: vec![],
        init_stack: vec![],
        pc: CodePointer::new(&Op::Finish),
        statistics: vec![Default::default(); 256],
        max_stack: 0,
    }
}

#[test]
fn op_finish() {
    let code = vec![Op::Finish];
    let mut vm = init_machine();
    vm.pc = CodePointer::new(&code[0]);
    unsafe {
        assert_eq!(vm.continue_running(), Scm::uninitialized());
    }
}
