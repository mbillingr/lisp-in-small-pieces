mod error;
mod op_impl;
mod opcode;
mod scheme_object_file;
mod value;

use bdwgc_alloc::Allocator;
use error::Result;
use opcode::Op;
use scheme_object_file::SchemeObjectFile;
use std::cell::Cell;
use std::time::{Duration, Instant};
use value::{Scm, Value, DYNENV_TAG};

#[cfg(not(feature = "disable-global-gc"))]
#[global_allocator]
static GLOBAL_ALLOCATOR: Allocator = Allocator;

fn main() -> Result<()> {
    unsafe { Allocator::initialize() }

    let sco = SchemeObjectFile::from_file("../test2.sco")?;

    println!("{:#?}", sco);

    let mut vm = VirtualMachine::from_sco(sco);

    unsafe {
        println!("Result: {}", vm.run());
    }

    unsafe {
        println!("Result: {}", vm.run());
    }

    let mut counts: Vec<_> = vm
        .statistics
        .iter()
        .enumerate()
        .map(|(i, n)| (*n, Op::from_u8(i as u8)))
        .collect();
    counts.sort_by_key(|(n, _)| *n);

    for c in counts.iter().rev().take(10) {
        println!("{:?}", c);
    }

    println!("max stack: {}", vm.max_stack);

    Ok(())
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct OpaquePointer(usize);

impl OpaquePointer {
    unsafe fn into<T: OpaqueCast>(self) -> T {
        T::from_op(self)
    }

    fn as_usize(&self) -> usize {
        self.0
    }
}

trait OpaqueCast {
    unsafe fn from_op(op: OpaquePointer) -> Self;
    fn as_op(&self) -> OpaquePointer;
}

#[derive(Debug, Clone, PartialEq)]
pub struct VirtualMachine {
    pc: CodePointer,
    val: Scm,
    fun: Scm,
    arg1: Scm,
    arg2: Scm,
    env: &'static ActivationFrame,
    constants: Vec<Scm>,
    mut_globals: Vec<Scm>,
    globals: Vec<Scm>,
    stack: Vec<OpaquePointer>,
    init_stack: Vec<OpaquePointer>,

    statistics: Vec<Duration>,
    max_stack: usize,
}

macro_rules! dispatch {
    ($s:ident.$code:ident) => {
        $s.$code();
    };
    ($s:ident.$code:ident($a:ident)) => {{
        let $a = $s.pc.fetch_byte();
        $s.$code($a);
    }};
    ($s:ident.$code:ident($a:ident, $b:ident)) => {{
        let $a = $s.pc.fetch_byte();
        let $b = $s.pc.fetch_byte();
        $s.$code($a, $b);
    }};
    ($s:ident.$code:ident()) => {
        $s.$code();
    };
    ($s:ident.$code:ident($a:expr)) => {{
        $s.$code($a);
    }};
    ($s:ident.$code:ident($a:expr, $b:expr)) => {{
        $s.$code($a, $b);
    }};
}

macro_rules! defprimitive {
    ($name:ident() $body:block) => {
        defprimitive!(stringify!($name), $body)
    };
    ($name:ident($a:ident) $body:block) => {
        defprimitive!(stringify!($name), $a, $body)
    };
    ($name:ident($a:ident, $b:ident) $body:block) => {
        defprimitive!(stringify!($name), $a, $b, $body)
    };
    ($name:ident($a:ident, $b:ident, $c:ident) $body:block) => {
        defprimitive!(stringify!($name), $a, $b, $c, $body)
    };

    ($name:expr, $body:block) => {{
        Scm::primitive(Primitive {
            name: $name,
            behavior: |vm| {
                let frame = vm.val.as_frame().unwrap();
                if frame.len() == 0 + 1 {
                    vm.val = $body;
                    unsafe {
                        vm.pc = vm.stack_pop();
                    }
                } else {
                    vm.signal_exception(concat!("incorrect arity: ", $name))
                }
            },
        })
    }};

    ($name:expr, $a:ident, $body:block) => {{
        Scm::primitive(Primitive {
            name: $name,
            behavior: |vm| {
                let frame = vm.val.as_frame().unwrap();
                if frame.len() == 1 + 1 {
                    let $a = *frame.argument(0);
                    vm.val = $body;
                    unsafe {
                        vm.pc = vm.stack_pop();
                    }
                } else {
                    vm.signal_exception(concat!("incorrect arity: ", $name))
                }
            },
        })
    }};

    ($name:expr, $a:ident, $b:ident, $body:block) => {{
        Scm::primitive(Primitive {
            name: $name,
            behavior: |vm| {
                let frame = vm.val.as_frame().unwrap();
                if frame.len() == 2 + 1 {
                    let $a = *frame.argument(0);
                    let $b = *frame.argument(1);
                    vm.val = $body;
                    unsafe {
                        vm.pc = vm.stack_pop();
                    }
                } else {
                    vm.signal_exception(concat!("incorrect arity: ", $name))
                }
            },
        })
    }};

    ($name:expr, $a:ident, $b:ident, $c:ident, $body:block) => {{
        Scm::primitive(Primitive {
            name: $name,
            behavior: |vm| {
                let frame = vm.val.as_frame().unwrap();
                if frame.len() == 2 + 1 {
                    let $a = *frame.argument(0);
                    let $b = *frame.argument(1);
                    let $c = *frame.argument(1);
                    vm.val = $body;
                    unsafe {
                        vm.pc = vm.stack_pop();
                    }
                } else {
                    vm.signal_exception(concat!("incorrect arity: ", $name))
                }
            },
        })
    }};
}

impl VirtualMachine {
    pub fn from_sco(sco: SchemeObjectFile) -> Self {
        let mut init_stack = vec![CodePointer::new_unchecked(&sco.bytecode[1]).as_op()]; // pc for FINISH
        for e in sco.entry_points.into_iter().rev() {
            init_stack.push(CodePointer::new_unchecked(&sco.bytecode[e]).as_op());
        }
        VirtualMachine {
            val: Scm::uninitialized(),
            fun: Scm::uninitialized(),
            arg1: Scm::uninitialized(),
            arg2: Scm::uninitialized(),
            env: ActivationFrame::allocate(0),
            constants: sco.constants,
            mut_globals: vec![Scm::uninitialized(); sco.global_vars.len()],
            globals: Self::init_predefined(),
            stack: Vec::with_capacity(1024),
            init_stack,
            pc: CodePointer::new(&Op::Finish),
            statistics: vec![Duration::new(0, 0); 256],
            max_stack: 0,
        }
    }

    fn init_predefined() -> Vec<Scm> {
        vec![
            Scm::bool(true),
            Scm::bool(false),
            Scm::null(),
            defprimitive!(cons(car, cdr) { Scm::cons(car, cdr) }),
            defprimitive!(car(x) { x.as_pair().unwrap().0 }),
            defprimitive!(cdr(x) { x.as_pair().unwrap().1 }),
            defprimitive!("pair?", x, { Scm::bool(x.is_pair()) }),
            defprimitive!("symbol?", x, { Scm::bool(x.is_symbol()) }),
            defprimitive!("eq?", a, b, { Scm::bool(a.eq(&b)) }),
            defprimitive!("null?", x, { Scm::bool(x.is_null()) }),
            defprimitive!("set-car!", x, a, {
                unsafe {
                    x.set_car(a);
                }
                Scm::uninitialized()
            }),
            defprimitive!("set-cdr!", x, d, {
                unsafe {
                    x.set_cdr(d);
                }
                Scm::uninitialized()
            }),
        ]
    }

    pub unsafe fn run(&mut self) -> Scm {
        self.stack.clear();
        for &i in &self.init_stack {
            self.stack.push(i)
        }
        self.pc = self.stack_pop();

        loop {
            self.max_stack = self.max_stack.max(self.stack.len());
            let op = self.pc.fetch_instruction();
            let start = Instant::now();
            match op {
                Op::Nop => dispatch!(self.nop),
                Op::ShallowArgumentRef0 => dispatch!(self.shallow_argument_ref(0)),
                Op::ShallowArgumentRef1 => dispatch!(self.shallow_argument_ref(21)),
                Op::ShallowArgumentRef2 => dispatch!(self.shallow_argument_ref(2)),
                Op::ShallowArgumentRef3 => dispatch!(self.shallow_argument_ref(3)),
                Op::ShallowArgumentRef => dispatch!(self.shallow_argument_ref(idx)),
                Op::DeepArgumentRef => dispatch!(self.deep_argument_ref(i, j)),
                Op::GlobalRef => dispatch!(self.global_ref(idx)),
                Op::CheckedGlobalRef => dispatch!(self.checked_global_ref(idx)),
                Op::Constant => dispatch!(self.constant(idx)),
                Op::Predefined0 => dispatch!(self.predefined0),
                Op::Predefined1 => dispatch!(self.predefined1),
                Op::Predefined2 => dispatch!(self.predefined2),
                Op::Predefined3 => dispatch!(self.predefined(3)),
                Op::Predefined4 => dispatch!(self.predefined(4)),
                Op::Predefined5 => dispatch!(self.predefined(5)),
                Op::Predefined6 => dispatch!(self.predefined(6)),
                Op::Predefined7 => dispatch!(self.predefined(7)),
                Op::Predefined8 => dispatch!(self.predefined(8)),
                Op::Predefined => dispatch!(self.predefined(idx)),
                Op::Finish => return self.val,
                Op::SetShallowArgument0 => dispatch!(self.set_shallow_argument(0)),
                Op::SetShallowArgument1 => dispatch!(self.set_shallow_argument(1)),
                Op::SetShallowArgument2 => dispatch!(self.set_shallow_argument(2)),
                Op::SetShallowArgument3 => dispatch!(self.set_shallow_argument(3)),
                Op::SetShallowArgument => dispatch!(self.set_shallow_argument(idx)),
                Op::SetDeepArgument => dispatch!(self.set_deep_argument(i, j)),
                Op::SetGlobal => dispatch!(self.set_global(idx)),
                Op::LongGoto => dispatch!(self.long_goto(lo, hi)),
                Op::LongJumpFalse => dispatch!(self.long_jump_false(lo, hi)),
                Op::ShortGoto => dispatch!(self.short_goto(lo)),
                Op::ShortJumpFalse => dispatch!(self.short_jump_false(lo)),
                Op::ExtendEnv => dispatch!(self.extend_env),
                Op::UnlinkEnv => dispatch!(self.unlink_env),
                Op::PushValue => dispatch!(self.push_value),
                Op::PopArg1 => dispatch!(self.pop_arg1),
                Op::PopArg2 => dispatch!(self.pop_arg2),
                Op::PreserveEnv => dispatch!(self.preserve_env),
                Op::RestoreEnv => dispatch!(self.restore_env),
                Op::PopFunction => dispatch!(self.pop_fun),
                Op::CreateClosure => dispatch!(self.create_closure(offset)),

                Op::Return => self.pc = self.stack_pop(),

                Op::FunctionInvoke => self.invoke(self.fun, false),
                Op::FunctionGoto => self.invoke(self.fun, true),

                Op::AllocateFrame1 => self.val = Scm::frame(1),
                Op::AllocateFrame2 => self.val = Scm::frame(2),
                Op::AllocateFrame3 => self.val = Scm::frame(3),
                Op::AllocateFrame4 => self.val = Scm::frame(4),
                Op::AllocateFrame5 => self.val = Scm::frame(5),
                Op::AllocateFrame => {
                    let size = self.pc.fetch_byte();
                    self.val = Scm::frame(size as usize);
                }

                Op::PopFrame0 => self
                    .val
                    .as_frame()
                    .unwrap()
                    .set_argument(0, self.stack_pop()),
                Op::PopFrame1 => self
                    .val
                    .as_frame()
                    .unwrap()
                    .set_argument(1, self.stack_pop()),
                Op::PopFrame2 => self
                    .val
                    .as_frame()
                    .unwrap()
                    .set_argument(2, self.stack_pop()),
                Op::PopFrame3 => self
                    .val
                    .as_frame()
                    .unwrap()
                    .set_argument(3, self.stack_pop()),
                Op::PopFrame => {
                    let rank = self.pc.fetch_byte();
                    self.val
                        .as_frame()
                        .unwrap()
                        .set_argument(rank as usize, self.stack_pop())
                }

                Op::IsArity1 => {
                    if self.val.as_frame().unwrap().len() != 1 {
                        self.signal_exception("Incorrect arity for nullary function")
                    }
                }
                Op::IsArity2 => {
                    if self.val.as_frame().unwrap().len() != 2 {
                        self.signal_exception("Incorrect arity for unary function")
                    }
                }
                Op::IsArity3 => {
                    if self.val.as_frame().unwrap().len() != 3 {
                        self.signal_exception("Incorrect arity for binary function")
                    }
                }
                Op::IsArity4 => {
                    if self.val.as_frame().unwrap().len() != 4 {
                        self.signal_exception("Incorrect arity for ternary function")
                    }
                }
                Op::IsArity => {
                    let rank = self.pc.fetch_byte();
                    if self.val.as_frame().unwrap().len() != rank as usize {
                        self.signal_exception("Incorrect arity")
                    }
                }

                Op::ShortNumber => {
                    let val = self.pc.fetch_byte();
                    self.val = Scm::int(val as i64);
                }
                Op::ConstMinus1 => self.val = Scm::int(-1),
                Op::Const0 => self.val = Scm::int(0),
                Op::Const1 => self.val = Scm::int(1),
                Op::Const2 => self.val = Scm::int(2),
                Op::Const4 => self.val = Scm::int(4),

                Op::Call2Cons => {
                    self.val = Scm::cons(self.arg1, self.val);
                }

                Op::Call2NumEq => {
                    self.val = self.arg1.numeq(&self.val);
                }
                Op::Call2Less => {
                    self.val = self.arg1.less(&self.val);
                }

                Op::Call2Add => self.val = self.arg1.add(&self.val),
                Op::Call2Sub => self.val = self.arg1.sub(&self.val),
                Op::Call2Mul => self.val = self.arg1.mul(&self.val),

                Op::DynamicRef => {
                    let index = self.pc.fetch_byte();
                    self.val = self.find_dynamic_value(index as usize);
                }
                Op::DynamicPop => self.pop_dynamic_binding(),
                Op::DynamicPush => {
                    let index = self.pc.fetch_byte();
                    self.push_dynamic_binding(index as usize, self.val);
                }

                op => unimplemented!("Opcode {:?}", op),
            }
            let end = Instant::now();
            self.statistics[op as usize] += end - start;
        }
    }

    fn stack_push<T: OpaqueCast>(&mut self, item: T) {
        self.stack.push(item.as_op())
    }

    unsafe fn stack_pop<T: OpaqueCast>(&mut self) -> T {
        T::from_op(self.stack.pop().unwrap())
    }

    fn invoke(&mut self, f: Scm, tail: bool) {
        if let Some(cls) = f.as_closure() {
            if !tail {
                self.stack_push(self.pc);
            }
            self.env = cls.closed_environment;
            self.pc = cls.code;
        } else if let Some(prim) = f.as_primitive() {
            if !tail {
                self.stack_push(self.pc);
            }
            (prim.behavior)(self)
        } else {
            self.signal_exception(format!("Not a function {:?}", f))
        }
    }

    fn signal_exception<T: ToString>(&self, msg: T) {
        unimplemented!("{}", msg.to_string())
    }

    fn global_fetch(&self, idx: usize) -> &Scm {
        &self.mut_globals[idx]
    }

    fn quotation_fetch(&self, idx: usize) -> &Scm {
        &self.constants[idx]
    }

    fn global_update(&mut self, idx: usize, value: Scm) {
        self.mut_globals[idx] = value;
    }

    unsafe fn push_dynamic_binding(&mut self, index: usize, value: Scm) {
        self.stack_push(Scm::int(self.search_dynenv_index() as i64));
        self.stack_push(value);
        self.stack_push(Scm::int(index as i64));
        self.stack_push(DYNENV_TAG);
    }

    fn pop_dynamic_binding(&mut self) {
        self.stack.pop();
        self.stack.pop();
        self.stack.pop();
        self.stack.pop();
    }

    unsafe fn find_dynamic_value(&self, index: usize) -> Scm {
        let index = Scm::int(index as i64);
        let mut idx = self.search_dynenv_index();
        loop {
            if idx == 0 {
                return Scm::uninitialized();
            }
            if self.stack[idx].into::<Scm>().eqv(&index) {
                return self.stack[idx - 1].into();
            }
            idx = self.stack[idx - 2].as_usize();
        }
    }

    unsafe fn search_dynenv_index(&self) -> usize {
        let mut idx = self.stack.len();
        loop {
            if idx == 0 {
                return 0;
            }
            idx -= 1;
            if self.stack[idx].into::<Scm>().eq(&DYNENV_TAG) {
                return idx - 1;
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CodePointer {
    ptr: *const u8,
}

impl CodePointer {
    fn new(code: &Op) -> Self {
        CodePointer {
            ptr: code as *const _ as *const u8,
        }
    }

    fn new_unchecked(code: &u8) -> Self {
        CodePointer { ptr: code }
    }

    unsafe fn fetch_instruction(&mut self) -> Op {
        // self.pc must always point to a valid code location
        unsafe { Op::from_u8_unchecked(self.fetch_byte()) }
    }

    unsafe fn fetch_byte(&mut self) -> u8 {
        // self.pc must always point to a valid code location
        let b = *self.ptr;
        self.ptr = self.ptr.offset(1);
        b
    }

    unsafe fn offset(&self, offset: isize) -> Self {
        CodePointer {
            ptr: self.ptr.offset(offset),
        }
    }

    unsafe fn jump(&mut self, offset: isize) {
        self.ptr = self.ptr.offset(offset);
    }
}

impl OpaqueCast for CodePointer {
    unsafe fn from_op(op: OpaquePointer) -> Self {
        CodePointer { ptr: op.0 as _ }
    }

    fn as_op(&self) -> OpaquePointer {
        OpaquePointer(self.ptr as usize)
    }
}

impl From<Value> for CodePointer {
    fn from(v: Value) -> Self {
        CodePointer {
            ptr: v.as_pointer().unwrap(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ActivationFrame {
    next: Cell<Option<&'static ActivationFrame>>,
    slots: Vec<Scm>,
    //slots: [Scm; 2],
}

impl ActivationFrame {
    fn new(size: usize) -> Self {
        ActivationFrame {
            next: Cell::new(None),
            slots: vec![Scm::uninitialized(); size],
        }
    }

    fn allocate(size: usize) -> &'static Self {
        Box::leak(Box::new(Self::new(size)))
    }

    fn len(&self) -> usize {
        self.slots.len()
    }

    fn next(&self) -> Option<&'static ActivationFrame> {
        self.next.get()
    }

    fn argument(&self, idx: usize) -> &Scm {
        &self.slots[idx]
    }

    fn set_argument(&self, idx: usize, value: Scm) {
        unsafe {
            *(&self.slots[idx] as *const _ as *mut _) = value;
        }
    }

    fn extend(&self, env: &'static ActivationFrame) -> &Self {
        self.next.set(Some(env));
        self
    }

    fn deep_fetch(&self, i: usize, j: usize) -> &Scm {
        let mut env = self;
        for _ in 0..i {
            env = env.next().unwrap();
        }
        env.argument(j)
    }

    fn deep_set(&self, i: usize, j: usize, value: Scm) {
        unsafe {
            *(self.deep_fetch(i, j) as *const _ as *mut _) = value;
        }
    }
}

impl OpaqueCast for &'static ActivationFrame {
    unsafe fn from_op(op: OpaquePointer) -> Self {
        &*(op.0 as *const _)
    }

    fn as_op(&self) -> OpaquePointer {
        OpaquePointer(*self as *const _ as _)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Closure {
    code: CodePointer,
    closed_environment: &'static ActivationFrame,
}

impl Closure {
    fn new(code: CodePointer, env: &'static ActivationFrame) -> Self {
        Closure {
            code,
            closed_environment: env,
        }
    }

    fn allocate(code: CodePointer, env: &'static ActivationFrame) -> &'static Self {
        Box::leak(Box::new(Self::new(code, env)))
    }
}

#[derive(Copy, Clone)]
pub struct Primitive {
    name: &'static str,
    behavior: fn(&mut VirtualMachine),
}
