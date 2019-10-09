use crate::ActivationFrame;
use crate::CodePointer;
use crate::Scm;
use crate::VirtualMachine;

impl VirtualMachine {
    #[inline(always)]
    pub fn nop(&mut self) {}

    #[inline(always)]
    pub fn shallow_argument_ref0(&mut self) {
        self.val = *self.env.argument(0);
    }

    #[inline(always)]
    pub fn shallow_argument_ref1(&mut self) {
        self.val = *self.env.argument(1);
    }

    #[inline(always)]
    pub fn shallow_argument_ref2(&mut self) {
        self.val = *self.env.argument(2);
    }

    #[inline(always)]
    pub fn shallow_argument_ref3(&mut self) {
        self.val = *self.env.argument(3);
    }

    #[inline(always)]
    pub fn shallow_argument_ref(&mut self, idx: u8) {
        self.val = *self.env.argument(idx as usize);
    }

    #[inline(always)]
    pub fn deep_argument_ref(&mut self, env: u8, idx: u8) {
        self.val = *self.env.deep_fetch(env as usize, idx as usize);
    }

    #[inline(always)]
    pub fn global_ref(&mut self, idx: u8) {
        self.val = *self.global_fetch(idx as usize);
    }

    #[inline(always)]
    pub fn checked_global_ref(&mut self, idx: u8) {
        self.global_ref(idx);
        if self.val.is_uninitialized() {
            self.signal_exception("Uninitialized global variable")
        }
    }

    #[inline(always)]
    pub fn constant(&mut self, idx: u8) {
        self.val = *self.quotation_fetch(idx as usize);
    }

    #[inline(always)]
    pub fn predefined0(&mut self) {
        self.val = Scm::bool(true);
    }

    #[inline(always)]
    pub fn predefined1(&mut self) {
        self.val = Scm::bool(false);
    }

    #[inline(always)]
    pub fn predefined2(&mut self) {
        self.val = Scm::null();
    }

    #[inline(always)]
    pub fn predefined3(&mut self) {
        self.val = self.globals[3];
    }

    #[inline(always)]
    pub fn predefined4(&mut self) {
        self.val = self.globals[4];
    }

    #[inline(always)]
    pub fn predefined5(&mut self) {
        self.val = self.globals[5];
    }

    #[inline(always)]
    pub fn predefined6(&mut self) {
        self.val = self.globals[6];
    }

    #[inline(always)]
    pub fn predefined7(&mut self) {
        self.val = self.globals[7];
    }

    #[inline(always)]
    pub fn predefined8(&mut self) {
        self.val = self.globals[8];
    }

    #[inline(always)]
    pub fn predefined(&mut self, idx: u8) {
        self.val = self.globals[idx as usize];
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Op;

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
    fn test_nop() {
        let mut vm = init_machine();
        vm.nop();
        assert_eq!(vm, init_machine());
    }

    #[test]
    fn test_shallow_argument_ref() {
        let mut vm = init_machine();
        vm.env = ActivationFrame::allocate(5);
        vm.env.set_argument(0, Scm::int(10));
        vm.env.set_argument(1, Scm::int(20));
        vm.env.set_argument(2, Scm::int(30));
        vm.env.set_argument(3, Scm::int(40));
        vm.env.set_argument(4, Scm::int(50));
        let mut reference = vm.clone();

        vm.shallow_argument_ref0();
        reference.val = Scm::int(10);
        assert_eq!(vm, reference);

        vm.shallow_argument_ref1();
        reference.val = Scm::int(20);
        assert_eq!(vm, reference);

        vm.shallow_argument_ref2();
        reference.val = Scm::int(30);
        assert_eq!(vm, reference);

        vm.shallow_argument_ref3();
        reference.val = Scm::int(40);
        assert_eq!(vm, reference);

        vm.shallow_argument_ref(4);
        reference.val = Scm::int(50);
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_deep_argument_ref() {
        let mut vm = init_machine();

        let mut env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));
        vm.env = env.extend(vm.env);

        let mut env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(10));
        env.set_argument(1, Scm::int(20));
        vm.env = env.extend(vm.env);

        let mut env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(100));
        env.set_argument(1, Scm::int(200));
        vm.env = env.extend(vm.env);

        let mut reference = vm.clone();

        vm.deep_argument_ref(0, 0);
        reference.val = Scm::int(100);
        assert_eq!(vm, reference);

        vm.deep_argument_ref(1, 1);
        reference.val = Scm::int(20);
        assert_eq!(vm, reference);

        vm.deep_argument_ref(2, 0);
        reference.val = Scm::int(1);
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_global_ref() {
        let mut vm = init_machine();
        vm.mut_globals = vec![Scm::int(42), Scm::int(666), Scm::int(-1)];
        let mut reference = vm.clone();

        vm.global_ref(0);
        reference.val = Scm::int(42);
        assert_eq!(vm, reference);

        vm.global_ref(1);
        reference.val = Scm::int(666);
        assert_eq!(vm, reference);

        vm.global_ref(2);
        reference.val = Scm::int(-1);
        assert_eq!(vm, reference);
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_checked_global_ref() {
        let mut vm = init_machine();
        vm.mut_globals = vec![Scm::uninitialized()];
        let mut reference = vm.clone();

        vm.checked_global_ref(0);
        reference.val = Scm::uninitialized();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_constant() {
        let mut vm = init_machine();
        vm.constants = vec![Scm::int(42), Scm::int(666), Scm::int(-1)];
        let mut reference = vm.clone();

        vm.constant(0);
        reference.val = Scm::int(42);
        assert_eq!(vm, reference);

        vm.constant(1);
        reference.val = Scm::int(666);
        assert_eq!(vm, reference);

        vm.constant(2);
        reference.val = Scm::int(-1);
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_predefined() {
        let mut vm = init_machine();
        vm.globals = vec![
            Scm::int(10),
            Scm::int(11),
            Scm::int(12),
            Scm::int(13),
            Scm::int(14),
        ];
        let mut reference = vm.clone();

        vm.predefined0();
        reference.val = Scm::bool(true);
        assert_eq!(vm, reference);

        vm.predefined1();
        reference.val = Scm::bool(false);
        assert_eq!(vm, reference);

        vm.predefined2();
        reference.val = Scm::null();
        assert_eq!(vm, reference);

        vm.predefined3();
        reference.val = Scm::int(13);
        assert_eq!(vm, reference);

        vm.predefined(1);
        reference.val = Scm::int(11);
        assert_eq!(vm, reference);
    }
}
