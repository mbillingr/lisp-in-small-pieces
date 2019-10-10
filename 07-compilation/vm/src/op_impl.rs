use crate::ActivationFrame;
use crate::CodePointer;
use crate::Scm;
use crate::Value;
use crate::VirtualMachine;

impl VirtualMachine {
    #[inline(always)]
    pub fn nop(&mut self) {}

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
    pub fn predefined(&mut self, idx: u8) {
        self.val = self.globals[idx as usize];
    }

    #[inline(always)]
    pub fn set_shallow_argument(&mut self, idx: u8) {
        self.env.set_argument(idx as usize, self.val);
    }

    #[inline(always)]
    pub fn set_deep_argument(&mut self, env: u8, idx: u8) {
        self.env.deep_set(env as usize, idx as usize, self.val);
    }

    #[inline(always)]
    pub fn set_global(&mut self, idx: u8) {
        self.global_update(idx as usize, self.val);
    }

    #[inline(always)]
    pub unsafe fn long_goto(&mut self, offset: u8, segment: u8) {
        self.pc = self.pc.offset(offset as isize + 256 * segment as isize)
    }

    #[inline(always)]
    pub unsafe fn long_jump_false(&mut self, offset: u8, segment: u8) {
        if self.val.is_false() {
            self.pc = self.pc.offset(offset as isize + 256 * segment as isize)
        }
    }

    #[inline(always)]
    pub unsafe fn short_goto(&mut self, offset: u8) {
        self.pc = self.pc.offset(offset as isize)
    }

    #[inline(always)]
    pub unsafe fn short_jump_false(&mut self, offset: u8) {
        if self.val.is_false() {
            self.pc = self.pc.offset(offset as isize)
        }
    }

    #[inline(always)]
    pub fn extend_env(&mut self) {
        self.env = &self.val.as_frame().unwrap().extend(self.env);
    }

    #[inline(always)]
    pub fn unlink_env(&mut self) {
        self.env = self.env.next().unwrap();
    }

    #[inline(always)]
    pub fn push_value(&mut self) {
        self.stack_push(self.val);
    }

    #[inline(always)]
    pub unsafe fn pop_arg1(&mut self) {
        self.arg1 = self.stack_pop();
    }

    #[inline(always)]
    pub unsafe fn pop_arg2(&mut self) {
        self.arg2 = self.stack_pop();
    }

    #[inline(always)]
    pub fn preserve_env(&mut self) {
        self.stack_push(self.env);
    }

    #[inline(always)]
    pub unsafe fn restore_env(&mut self) {
        self.env = self.stack_pop();
    }

    #[inline(always)]
    pub unsafe fn pop_fun(&mut self) {
        self.fun = self.stack_pop();
    }

    #[inline(always)]
    pub unsafe fn create_closure(&mut self, offset: u8) {
        self.val = Scm::closure(self.pc.offset(offset as isize), self.env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Op;
    use crate::OpaqueCast;

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

        vm.shallow_argument_ref(0);
        reference.val = Scm::int(10);
        assert_eq!(vm, reference);

        vm.shallow_argument_ref(1);
        reference.val = Scm::int(20);
        assert_eq!(vm, reference);

        vm.shallow_argument_ref(2);
        reference.val = Scm::int(30);
        assert_eq!(vm, reference);

        vm.shallow_argument_ref(3);
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

        vm.predefined(3);
        reference.val = Scm::int(13);
        assert_eq!(vm, reference);

        vm.predefined(1);
        reference.val = Scm::int(11);
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_set_shallow_argument() {
        let mut vm = init_machine();
        vm.env = ActivationFrame::allocate(5);
        vm.env.set_argument(0, Scm::int(10));
        vm.env.set_argument(1, Scm::int(20));
        vm.env.set_argument(2, Scm::int(30));
        vm.env.set_argument(3, Scm::int(40));
        vm.env.set_argument(4, Scm::int(50));
        let mut reference = vm.clone();

        vm.val = Scm::int(50);
        vm.set_shallow_argument(0);

        vm.val = Scm::int(40);
        vm.set_shallow_argument(1);

        vm.val = Scm::int(30);
        vm.set_shallow_argument(2);

        vm.val = Scm::int(20);
        vm.set_shallow_argument(3);

        vm.val = Scm::int(10);
        vm.set_shallow_argument(4);

        vm.val = Scm::int(0);
        reference.val = Scm::int(0);
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_set_deep_argument() {
        let mut vm = init_machine();

        let mut env1 = ActivationFrame::allocate(2);
        let mut env2 = ActivationFrame::allocate(2);
        let mut env3 = ActivationFrame::allocate(2);
        env1.extend(vm.env);
        env2.extend(env1);
        vm.env = env3.extend(env2);

        let mut reference = vm.clone();

        env1.set_argument(0, Scm::int(1));
        env1.set_argument(1, Scm::int(2));
        env2.set_argument(0, Scm::int(10));
        env2.set_argument(1, Scm::int(20));
        env3.set_argument(0, Scm::int(100));
        env3.set_argument(1, Scm::int(200));

        vm.val = Scm::int(100);
        vm.set_deep_argument(0, 0);

        vm.val = Scm::int(200);
        vm.set_deep_argument(0, 1);

        vm.val = Scm::int(10);
        vm.set_deep_argument(1, 0);

        vm.val = Scm::int(20);
        vm.set_deep_argument(1, 1);

        vm.val = Scm::int(1);
        vm.set_deep_argument(2, 0);

        vm.val = Scm::int(2);
        vm.set_deep_argument(2, 1);

        vm.val = Scm::int(0);
        reference.val = Scm::int(0);
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_set_global() {
        let mut vm = init_machine();
        vm.mut_globals = vec![
            Scm::uninitialized(),
            Scm::uninitialized(),
            Scm::uninitialized(),
        ];

        let mut reference = init_machine();
        reference.mut_globals = vec![Scm::int(42), Scm::int(666), Scm::int(-1)];

        vm.val = Scm::int(42);
        vm.set_global(0);

        vm.val = Scm::int(666);
        vm.set_global(1);

        vm.val = Scm::int(-1);
        vm.set_global(2);

        vm.val = Scm::int(0);
        reference.val = Scm::int(0);
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_long_goto() {
        let code = vec![Op::Nop; 1024];

        let mut vm = init_machine();
        vm.pc = CodePointer::new(&code[0]);

        let mut reference = init_machine();
        reference.pc = CodePointer::new(&code[1000]);

        unsafe {
            vm.long_goto(232, 3); // 1000 = 232 + 3*256
        }

        assert_eq!(vm, reference);
    }

    #[test]
    fn test_long_jmup_false() {
        let code = vec![Op::Nop; 1024];

        let mut vm = init_machine();
        vm.pc = CodePointer::new(&code[0]);

        let mut ref_true = init_machine();
        ref_true.pc = CodePointer::new(&code[0]);

        let mut ref_false = init_machine();
        ref_false.pc = CodePointer::new(&code[1000]);

        vm.val = Scm::bool(true);
        unsafe {
            vm.long_jump_false(232, 3);
        }
        vm.val = Scm::uninitialized();
        assert_eq!(vm, ref_true);

        vm.val = Scm::bool(false);
        unsafe {
            vm.long_jump_false(232, 3); // 1000 = 232 + 3*256
        }
        vm.val = Scm::uninitialized();
        assert_eq!(vm, ref_false);
    }

    #[test]
    fn test_short_goto() {
        let code = vec![Op::Nop; 1024];

        let mut vm = init_machine();
        vm.pc = CodePointer::new(&code[0]);

        let mut reference = init_machine();
        reference.pc = CodePointer::new(&code[200]);

        unsafe {
            vm.short_goto(200);
        }

        assert_eq!(vm, reference);
    }

    #[test]
    fn test_short_jmup_false() {
        let code = vec![Op::Nop; 1024];

        let mut vm = init_machine();
        vm.pc = CodePointer::new(&code[0]);

        let mut ref_true = init_machine();
        ref_true.pc = CodePointer::new(&code[0]);

        let mut ref_false = init_machine();
        ref_false.pc = CodePointer::new(&code[200]);

        vm.val = Scm::bool(true);
        unsafe {
            vm.short_jump_false(200);
        }
        vm.val = Scm::uninitialized();
        assert_eq!(vm, ref_true);

        vm.val = Scm::bool(false);
        unsafe {
            vm.short_jump_false(200);
        }
        vm.val = Scm::uninitialized();
        assert_eq!(vm, ref_false);
    }

    #[test]
    fn test_extend_env() {
        let mut reference = init_machine();
        let mut env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));
        reference.env = env.extend(reference.env);

        let mut vm = init_machine();
        let mut env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));

        vm.val = Scm::from_value(Value::Frame(env));
        vm.extend_env();
        vm.val = Scm::uninitialized();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_unlink_env() {
        let mut reference = init_machine();

        let mut vm = init_machine();
        let mut env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));
        vm.env = env.extend(vm.env);

        vm.unlink_env();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_push_value() {
        let mut reference = init_machine();
        reference.stack = vec![
            Scm::uninitialized().as_op(),
            Scm::int(1).as_op(),
            Scm::int(23).as_op(),
        ];

        let mut vm = init_machine();

        vm.push_value();
        vm.val = Scm::int(1);
        vm.push_value();
        vm.val = Scm::int(23);
        vm.push_value();
        vm.val = Scm::uninitialized();

        assert_eq!(vm, reference);
    }

    #[test]
    fn test_pop_arg() {
        let mut reference = init_machine();
        reference.arg1 = Scm::int(123);
        reference.arg2 = Scm::int(456);
        reference.fun = Scm::int(789);

        let mut vm = init_machine();
        vm.stack = vec![
            Scm::int(123).as_op(),
            Scm::int(456).as_op(),
            Scm::int(789).as_op(),
        ];

        unsafe {
            vm.pop_fun();
            vm.pop_arg2();
            vm.pop_arg1();
        }

        assert_eq!(vm, reference);
    }

    #[test]
    fn test_preserve_env() {
        let mut reference = init_machine();

        let mut env = ActivationFrame::allocate(3);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));
        env.set_argument(2, Scm::int(4));
        reference.env = env.extend(reference.env);
        reference.stack = vec![env.as_op()];

        let mut vm = init_machine();
        vm.env = env.extend(vm.env);

        vm.preserve_env();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_restore_env() {
        let mut reference = init_machine();

        let mut vm = init_machine();
        vm.stack = vec![vm.env.as_op()];
        let mut env = ActivationFrame::allocate(3);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));
        env.set_argument(2, Scm::int(4));
        vm.env = env.extend(vm.env);

        unsafe {
            vm.restore_env();
        }
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_create_closure() {
        let code = vec![Op::Nop; 32];

        let mut vm = init_machine();
        vm.pc = CodePointer::new(&code[0]);
        unsafe {
            vm.create_closure(3);
        }
        assert!(vm.val.is_closure());
        assert_eq!(
            vm.val.as_closure().unwrap().code,
            CodePointer::new(&code[3])
        );
    }
}
