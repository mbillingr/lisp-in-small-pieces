use crate::types::scm::ESCAPE_TAG;
use crate::types::{ActivationFrame, Scm, ScmBoxedValue};
use crate::vm::VirtualMachine;

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
            self.signal_exception_str(true, "Uninitialized global variable")
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
        self.env = &self.val.as_frame().unwrap().extends(self.env);
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
        self.arg1 = self.stack_pop_into();
    }

    #[inline(always)]
    pub unsafe fn pop_arg2(&mut self) {
        self.arg2 = self.stack_pop_into();
    }

    #[inline(always)]
    pub fn preserve_env(&mut self) {
        self.stack_push(self.env);
    }

    #[inline(always)]
    pub unsafe fn restore_env(&mut self) {
        self.env = self.stack_pop_into();
    }

    #[inline(always)]
    pub unsafe fn pop_fun(&mut self) {
        self.fun = self.stack_pop_into();
    }

    #[inline(always)]
    pub unsafe fn create_closure(&mut self, offset: u8) {
        self.val = Scm::closure(self.pc.offset(offset as isize), self.env)
    }

    #[inline(always)]
    pub unsafe fn return_(&mut self) {
        self.pc = self.stack_pop_into();
    }

    #[inline(always)]
    pub fn pack_frame(&mut self, arity: u8) {
        self.val.as_frame().unwrap().listify(arity as usize)
    }

    #[inline(always)]
    pub fn function_invoke(&mut self) {
        self.invoke(self.fun, false);
    }

    #[inline(always)]
    pub fn function_goto(&mut self) {
        self.invoke(self.fun, true);
    }

    #[inline(always)]
    pub unsafe fn pop_cons_frame(&mut self, arity: u8) {
        let arity = arity as usize;
        let frame = self.val.as_frame().unwrap();
        frame.set_argument(
            arity,
            Scm::cons(self.stack_pop_into(), *frame.argument(arity)),
        )
    }

    #[inline(always)]
    pub fn allocate_frame(&mut self, size: u8) {
        self.val = Scm::frame(size as usize);
    }

    #[inline(always)]
    pub fn allocate_dotted_frame(&mut self, size: u8) {
        let size = size as usize;
        let frame = ActivationFrame::allocate(size);
        frame.set_argument(size - 1, Scm::null());
        self.val = Scm::from_value(ScmBoxedValue::Frame(frame));
    }

    #[inline(always)]
    pub unsafe fn pop_frame(&mut self, idx: u8) {
        self.val
            .as_frame()
            .unwrap()
            .set_argument(idx as usize, self.stack_pop_into())
    }

    #[inline(always)]
    pub fn is_arity(&mut self, rank: u8) {
        if self.val.as_frame().unwrap().len() != rank as usize {
            self.signal_exception_str(
                false,
                match rank {
                    1 => "Incorrect arity for nullary function",
                    2 => "Incorrect arity for unary function",
                    3 => "Incorrect arity for binary function",
                    4 => "Incorrect arity for ternary function",
                    _ => "Incorrect arity",
                },
            )
        }
    }

    #[inline(always)]
    pub fn is_arity_greater(&mut self, rank: u8) {
        if self.val.as_frame().unwrap().len() < rank as usize {
            self.signal_exception_str(false, "Too few function arguments");
        }
    }

    #[inline(always)]
    pub fn short_number(&mut self, value: u8) {
        self.val = Scm::int(value as i64);
    }

    #[inline(always)]
    pub fn call1_car(&mut self) {
        self.val = self.val.car().expect("Not a pair");
    }

    #[inline(always)]
    pub fn call1_cdr(&mut self) {
        self.val = self.val.cdr().expect("Not a pair");
    }

    #[inline(always)]
    pub fn call1_is_pair(&mut self) {
        self.val = Scm::bool(self.val.is_pair());
    }

    #[inline(always)]
    pub fn call1_is_symbol(&mut self) {
        self.val = Scm::bool(self.val.is_symbol());
    }

    #[inline(always)]
    pub fn call1_display(&mut self) {
        println!("{}", self.val);
        self.val = Scm::uninitialized();
    }

    #[inline(always)]
    pub fn call1_is_null(&mut self) {
        self.val = Scm::bool(self.val.is_null());
    }

    #[inline(always)]
    pub fn call2_cons(&mut self) {
        self.val = Scm::cons(self.arg1, self.val);
    }

    #[inline(always)]
    pub fn call2_eq(&mut self) {
        self.val = Scm::bool(Scm::eq(&self.arg1, &self.val));
    }

    #[inline(always)]
    pub fn call2_set_car(&mut self) {
        if !self.arg1.is_pair() {
            panic!("Not a pair");
        }
        unsafe {
            self.arg1.set_car_unchecked(self.val);
        }
        self.val = Scm::uninitialized();
    }

    #[inline(always)]
    pub fn call2_set_cdr(&mut self) {
        if !self.arg1.is_pair() {
            panic!("Not a pair");
        }
        unsafe {
            self.arg1.set_cdr_unchecked(self.val);
        }
        self.val = Scm::uninitialized();
    }

    #[inline(always)]
    pub fn call2_numeq(&mut self) {
        self.val = Scm::bool(Scm::numeq(&self.arg1, &self.val));
    }

    #[inline(always)]
    pub fn call2_less(&mut self) {
        self.val = Scm::bool(Scm::less(&self.arg1, &self.val));
    }

    #[inline(always)]
    pub fn call2_less_eq(&mut self) {
        self.val = Scm::bool(Scm::less_eq(&self.arg1, &self.val));
    }

    #[inline(always)]
    pub fn call2_greater(&mut self) {
        self.val = Scm::bool(Scm::greater(&self.arg1, &self.val));
    }

    #[inline(always)]
    pub fn call2_greater_eq(&mut self) {
        self.val = Scm::bool(Scm::greater_eq(&self.arg1, &self.val));
    }

    #[inline(always)]
    pub fn call2_add(&mut self) {
        self.val = self.arg1.add(self.val);
    }

    #[inline(always)]
    pub fn call2_sub(&mut self) {
        self.val = self.arg1.sub(&self.val);
    }

    #[inline(always)]
    pub fn call2_mul(&mut self) {
        self.val = self.arg1.mul(&self.val);
    }

    #[inline(always)]
    pub fn call2_div(&mut self) {
        self.val = self.arg1.div(&self.val);
    }

    #[inline(always)]
    pub unsafe fn dynamic_ref(&mut self, idx: u8) {
        self.val = self.find_dynamic_value(idx as usize)
    }

    #[inline(always)]
    pub unsafe fn dynamic_pop(&mut self) {
        self.pop_dynamic_binding();
    }

    #[inline(always)]
    pub fn dynamic_push(&mut self, idx: u8) {
        self.push_dynamic_binding(idx as usize, self.val);
    }

    #[inline(always)]
    pub unsafe fn non_cont_err(&mut self) {
        self.signal_exception_str(false, "Attempt to continue non-continuable exception");
    }

    #[inline(always)]
    pub unsafe fn push_handler(&mut self) {
        self.push_exception_handler(self.val);
    }

    #[inline(always)]
    pub fn pop_handler(&mut self) {
        self.pop_exception_handler();
    }

    #[inline(always)]
    pub unsafe fn pop_escaper(&mut self) {
        let _tag = self.stack_pop();
        let _escape = self.stack_pop();
        self.restore_env();
    }

    #[inline(always)]
    pub unsafe fn push_escaper(&mut self, offset: u8) {
        self.preserve_env();
        let escape = Scm::escape(self.stack.len() + 3);
        let frame = ActivationFrame::allocate(1);
        frame.set_argument(0, escape);
        self.env = frame.extends(self.env);
        self.stack_push(escape);
        self.stack_push(ESCAPE_TAG);
        self.stack_push(self.pc.offset(offset as isize))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{CodePointer, OpaqueCast, Primitive};
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

        let env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));
        vm.env = env.extends(vm.env);

        let env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(10));
        env.set_argument(1, Scm::int(20));
        vm.env = env.extends(vm.env);

        let env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(100));
        env.set_argument(1, Scm::int(200));
        vm.env = env.extends(vm.env);

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
    #[should_panic(expected = "Uninitialized global variable")]
    fn test_checked_global_ref() {
        let mut vm = init_machine();
        vm.prepare_stack();
        vm.mut_globals = vec![Scm::uninitialized()];

        vm.checked_global_ref(0);
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

        let env1 = ActivationFrame::allocate(2);
        let env2 = ActivationFrame::allocate(2);
        let env3 = ActivationFrame::allocate(2);
        env1.extends(vm.env);
        env2.extends(env1);
        vm.env = env3.extends(env2);

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
        let env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));
        reference.env = env.extends(reference.env);

        let mut vm = init_machine();
        let env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));

        vm.val = Scm::from_value(ScmBoxedValue::Frame(env));
        vm.extend_env();
        vm.val = Scm::uninitialized();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_unlink_env() {
        let reference = init_machine();

        let mut vm = init_machine();
        let env = ActivationFrame::allocate(2);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));
        vm.env = env.extends(vm.env);

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

        let env = ActivationFrame::allocate(3);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));
        env.set_argument(2, Scm::int(4));
        reference.env = env.extends(reference.env);
        reference.stack = vec![env.as_op()];

        let mut vm = init_machine();
        vm.env = env.extends(vm.env);

        vm.preserve_env();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_restore_env() {
        let reference = init_machine();

        let mut vm = init_machine();
        vm.stack = vec![vm.env.as_op()];
        let env = ActivationFrame::allocate(3);
        env.set_argument(0, Scm::int(1));
        env.set_argument(1, Scm::int(2));
        env.set_argument(2, Scm::int(4));
        vm.env = env.extends(vm.env);

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
            *vm.val.as_closure().unwrap().code(),
            CodePointer::new(&code[3])
        );
    }

    #[test]
    fn test_return() {
        let code = vec![Op::Nop; 32];

        let mut reference = init_machine();
        reference.pc = CodePointer::new(&code[5]);

        let mut vm = init_machine();
        vm.pc = CodePointer::new(&code[15]);
        vm.stack = vec![CodePointer::new(&code[5]).as_op()];

        unsafe {
            vm.return_();
        }

        assert_eq!(vm, reference);
    }

    #[test]
    fn test_pack_frame() {
        let ref_frame = ActivationFrame::new(5);
        ref_frame.set_argument(0, Scm::int(1));
        ref_frame.set_argument(1, Scm::int(2));
        ref_frame.set_argument(2, Scm::int(3));
        ref_frame.set_argument(
            3,
            Scm::cons(Scm::int(4), Scm::cons(Scm::int(5), Scm::null())),
        );
        ref_frame.set_argument(4, Scm::int(5));

        let mut vm = init_machine();
        vm.val = Scm::frame(5);
        let frame = vm.val.as_frame().unwrap();
        frame.set_argument(0, Scm::int(1));
        frame.set_argument(1, Scm::int(2));
        frame.set_argument(2, Scm::int(3));
        frame.set_argument(3, Scm::int(4));
        frame.set_argument(4, Scm::int(5));

        vm.pack_frame(3);
        assert_eq!(
            vm.val.as_frame().unwrap().argument(0),
            ref_frame.argument(0)
        );
        assert_eq!(
            vm.val.as_frame().unwrap().argument(1),
            ref_frame.argument(1)
        );
        assert_eq!(
            vm.val.as_frame().unwrap().argument(2),
            ref_frame.argument(2)
        );
        assert!(vm
            .val
            .as_frame()
            .unwrap()
            .argument(3)
            .equal(&ref_frame.argument(3)));
        assert_eq!(
            vm.val.as_frame().unwrap().argument(4),
            ref_frame.argument(4)
        );
    }

    #[test]
    #[should_panic(expected = "Not a function")]
    fn test_function_invoke() {
        let mut vm = init_machine();
        vm.prepare_stack();
        vm.function_invoke();
    }

    #[test]
    #[should_panic(expected = "Not a function")]
    fn test_function_goto() {
        let mut vm = init_machine();
        vm.prepare_stack();
        vm.function_goto();
    }

    #[test]
    fn test_pop_cons_frame() {
        let ref_frame = ActivationFrame::new(1);
        ref_frame.set_argument(
            0,
            Scm::cons(Scm::int(2), Scm::cons(Scm::int(3), Scm::int(1))),
        );

        let mut vm = init_machine();
        vm.stack = vec![Scm::int(2).as_op(), Scm::int(3).as_op()];
        vm.val = Scm::frame(1);
        let frame = vm.val.as_frame().unwrap();
        frame.set_argument(0, Scm::int(1));

        unsafe {
            vm.pop_cons_frame(0);
            vm.pop_cons_frame(0);
        }
        assert!(vm
            .val
            .as_frame()
            .unwrap()
            .argument(0)
            .equal(&ref_frame.argument(0)));
    }

    #[test]
    fn test_allocate_frame() {
        let mut vm = init_machine();

        vm.allocate_frame(10);
        assert_eq!(vm.val.as_frame().map(|frame| frame.len()), Some(10));
    }

    #[test]
    fn test_allocate_dotted_frame() {
        let mut vm = init_machine();

        vm.allocate_dotted_frame(10);
        assert_eq!(vm.val.as_frame().map(|frame| frame.len()), Some(10));
        assert_eq!(*vm.val.as_frame().unwrap().argument(9), Scm::null());
    }

    #[test]
    fn test_pop_frame() {
        let ref_frame = ActivationFrame::new(2);
        ref_frame.set_argument(0, Scm::int(42));
        ref_frame.set_argument(1, Scm::int(666));

        let mut vm = init_machine();
        vm.stack = vec![Scm::int(42).as_op(), Scm::int(666).as_op()];
        vm.val = Scm::frame(2);

        unsafe {
            vm.pop_frame(1);
            vm.pop_frame(0);
        }
        assert_eq!(vm.val.as_frame().unwrap().len(), ref_frame.len());
        assert_eq!(
            vm.val.as_frame().unwrap().argument(0),
            ref_frame.argument(0)
        );
        assert_eq!(
            vm.val.as_frame().unwrap().argument(1),
            ref_frame.argument(1)
        );
    }

    #[test]
    fn test_call1car() {
        let mut reference = init_machine();
        reference.val = Scm::int(6);

        let mut vm = init_machine();
        vm.val = Scm::cons(Scm::int(6), Scm::int(7));

        vm.call1_car();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_call1cdr() {
        let mut reference = init_machine();
        reference.val = Scm::int(7);

        let mut vm = init_machine();
        vm.val = Scm::cons(Scm::int(6), Scm::int(7));

        vm.call1_cdr();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_call1_is_pair() {
        let mut reference = init_machine();
        reference.val = Scm::bool(true);

        let mut vm = init_machine();
        vm.val = Scm::cons(Scm::int(6), Scm::int(7));

        vm.call1_is_pair();
        assert_eq!(vm, reference);

        let mut reference = init_machine();
        reference.val = Scm::bool(false);

        let mut vm = init_machine();
        vm.val = Scm::null();

        vm.call1_is_pair();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_call1_is_symbol() {
        let mut reference = init_machine();
        reference.val = Scm::bool(true);

        let mut vm = init_machine();
        vm.val = Scm::symbol("foo");

        vm.call1_is_symbol();
        assert_eq!(vm, reference);

        let mut reference = init_machine();
        reference.val = Scm::bool(false);

        let mut vm = init_machine();
        vm.val = Scm::string("bar");

        vm.call1_is_symbol();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_call1_is_nulll() {
        let mut reference = init_machine();
        reference.val = Scm::bool(true);

        let mut vm = init_machine();
        vm.val = Scm::null();

        vm.call1_is_null();
        assert_eq!(vm, reference);

        let mut reference = init_machine();
        reference.val = Scm::bool(false);

        let mut vm = init_machine();
        vm.val = Scm::uninitialized();

        vm.call1_is_null();
        assert_eq!(vm, reference);
    }

    #[test]
    fn test_call2_cons() {
        let mut vm = init_machine();

        vm.val = Scm::null();
        vm.arg1 = Scm::int(42);
        vm.call2_cons();
        assert!(vm.val.equal(&Scm::cons(Scm::int(42), Scm::null())));
    }

    #[test]
    fn test_call2_eq() {
        let mut vm = init_machine();

        vm.val = Scm::cons(Scm::int(42), Scm::int(0));
        vm.arg1 = vm.val;
        vm.call2_eq();
        assert!(vm.val.as_bool());

        vm.val = Scm::cons(Scm::int(42), Scm::int(0));
        vm.arg1 = Scm::cons(Scm::int(42), Scm::int(0));
        vm.call2_eq();
        assert!(!vm.val.as_bool());
    }

    #[test]
    fn test_call2_set_car() {
        let mut vm = init_machine();

        let pair = Scm::cons(Scm::int(42), Scm::int(0));

        vm.val = Scm::int(123);
        vm.arg1 = pair;
        vm.call2_set_car();
        assert_eq!(pair.car(), Some(Scm::int(123)));
    }

    #[test]
    fn test_call2_set_cdr() {
        let mut vm = init_machine();

        let pair = Scm::cons(Scm::int(42), Scm::int(0));

        vm.val = Scm::int(456);
        vm.arg1 = pair;
        vm.call2_set_cdr();
        assert_eq!(pair.cdr(), Some(Scm::int(456)));
    }

    #[test]
    fn test_call2_numeq() {
        let mut vm = init_machine();

        vm.arg1 = Scm::int(42);
        vm.val = Scm::int(42);
        vm.call2_numeq();
        assert!(vm.val.as_bool());

        vm.arg1 = Scm::int(34);
        vm.val = Scm::int(12);
        vm.call2_numeq();
        assert!(!vm.val.as_bool());
    }

    #[test]
    fn test_call2_less() {
        let mut vm = init_machine();

        vm.arg1 = Scm::int(42);
        vm.val = Scm::int(42);
        vm.call2_less();
        assert!(!vm.val.as_bool());

        vm.arg1 = Scm::int(34);
        vm.val = Scm::int(56);
        vm.call2_less();
        assert!(vm.val.as_bool());

        vm.arg1 = Scm::int(34);
        vm.val = Scm::int(12);
        vm.call2_less();
        assert!(!vm.val.as_bool());
    }

    #[test]
    fn test_call2_less_eq() {
        let mut vm = init_machine();

        vm.arg1 = Scm::int(42);
        vm.val = Scm::int(42);
        vm.call2_less_eq();
        assert!(vm.val.as_bool());

        vm.arg1 = Scm::int(34);
        vm.val = Scm::int(56);
        vm.call2_less_eq();
        assert!(vm.val.as_bool());

        vm.arg1 = Scm::int(34);
        vm.val = Scm::int(12);
        vm.call2_less_eq();
        assert!(!vm.val.as_bool());
    }

    #[test]
    fn test_call2_greater() {
        let mut vm = init_machine();

        vm.arg1 = Scm::int(42);
        vm.val = Scm::int(42);
        vm.call2_greater();
        assert!(!vm.val.as_bool());

        vm.arg1 = Scm::int(34);
        vm.val = Scm::int(56);
        vm.call2_greater();
        assert!(!vm.val.as_bool());

        vm.arg1 = Scm::int(34);
        vm.val = Scm::int(12);
        vm.call2_greater();
        assert!(vm.val.as_bool());
    }

    #[test]
    fn test_call2_greater_eq() {
        let mut vm = init_machine();

        vm.arg1 = Scm::int(42);
        vm.val = Scm::int(42);
        vm.call2_greater_eq();
        assert!(vm.val.as_bool());

        vm.arg1 = Scm::int(34);
        vm.val = Scm::int(56);
        vm.call2_greater_eq();
        assert!(!vm.val.as_bool());

        vm.arg1 = Scm::int(34);
        vm.val = Scm::int(12);
        vm.call2_greater_eq();
        assert!(vm.val.as_bool());
    }

    #[test]
    fn test_call2_add() {
        let mut vm = init_machine();

        vm.arg1 = Scm::int(123);
        vm.val = Scm::int(456);
        vm.call2_add();
        assert_eq!(vm.val, Scm::int(579));

        vm.arg1 = Scm::int(-12);
        vm.val = Scm::int(2);
        vm.call2_add();
        assert_eq!(vm.val, Scm::int(-10));
    }

    #[test]
    fn test_call2_sub() {
        let mut vm = init_machine();

        vm.arg1 = Scm::int(123);
        vm.val = Scm::int(456);
        vm.call2_sub();
        assert_eq!(vm.val, Scm::int(-333));

        vm.arg1 = Scm::int(12);
        vm.val = Scm::int(-8);
        vm.call2_sub();
        assert_eq!(vm.val, Scm::int(20));
    }

    #[test]
    fn test_call2_mul() {
        let mut vm = init_machine();

        vm.arg1 = Scm::int(123);
        vm.val = Scm::int(456);
        vm.call2_mul();
        assert_eq!(vm.val, Scm::int(56088));

        vm.arg1 = Scm::int(12);
        vm.val = Scm::int(-3);
        vm.call2_mul();
        assert_eq!(vm.val, Scm::int(-36));
    }

    #[test]
    fn test_call2_div() {
        let mut vm = init_machine();

        vm.arg1 = Scm::int(246);
        vm.val = Scm::int(123);
        vm.call2_div();
        assert_eq!(vm.val, Scm::int(2));

        vm.arg1 = Scm::int(12);
        vm.val = Scm::int(-3);
        vm.call2_div();
        assert_eq!(vm.val, Scm::int(-4));
    }

    #[test]
    fn test_dynamic_ref() {
        let mut vm = init_machine();
        vm.push_dynamic_binding(1, Scm::int(0));
        vm.push_dynamic_binding(2, Scm::int(42));
        vm.push_dynamic_binding(1, Scm::int(10));

        unsafe {
            vm.dynamic_ref(2);
        }
        assert_eq!(vm.val, Scm::int(42));
        unsafe {
            vm.dynamic_ref(1);
        }
        assert_eq!(vm.val, Scm::int(10));
    }

    #[test]
    fn test_dynamic_pop() {
        let mut vm = init_machine();
        vm.push_dynamic_binding(1, Scm::int(0));
        vm.push_dynamic_binding(2, Scm::int(42));
        vm.push_dynamic_binding(1, Scm::int(10));

        vm.pop_dynamic_binding();

        unsafe {
            vm.dynamic_ref(1);
        }
        assert_eq!(vm.val, Scm::int(0));
    }

    #[test]
    fn test_dynamic_push() {
        let mut vm = init_machine();

        vm.val = Scm::int(123);
        vm.dynamic_push(255);
        vm.val = Scm::uninitialized();

        unsafe {
            vm.dynamic_ref(255);
        }
        assert_eq!(vm.val, Scm::int(123));
    }

    #[test]
    fn test_push_handler() {
        let mut vm = init_machine();
        vm.prepare_stack();

        static HANDLED: Scm = Scm::int(123456789);

        vm.val = Scm::primitive(Primitive::new("my handler", |vm| {
            vm.val = HANDLED;
        }));
        unsafe {
            vm.push_handler();
        }

        vm.val = Scm::int(123);
        vm.function_goto();
        unsafe {
            assert_eq!(vm.continue_running(), HANDLED);
        }
    }

    #[test]
    #[should_panic(expected = "Unhandled exception")]
    fn test_pop_handler() {
        let mut vm = init_machine();
        vm.prepare_stack();

        static HANDLED: Scm = Scm::int(123456789);

        vm.val = Scm::primitive(Primitive::new("my handler", |vm| {
            vm.val = HANDLED;
        }));
        unsafe {
            vm.push_handler();
        }

        vm.pop_handler();

        vm.val = Scm::int(123);
        vm.function_goto();
        unsafe {
            assert_eq!(vm.continue_running(), HANDLED);
        }
    }

    #[test]
    fn test_push_escaper() {
        let mut vm = init_machine();
        unsafe {
            vm.push_escaper(5);
        }
        assert!(vm.env.argument(0).is_escape());
    }

    #[test]
    fn test_pop_escaper() {
        let mut vm = init_machine();
        let env = vm.env;
        unsafe {
            vm.push_escaper(5);
            vm.return_();
            vm.pop_escaper();
        }
        assert_eq!(vm.env, env);
    }
}
