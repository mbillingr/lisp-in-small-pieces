mod error;
mod opcode;
mod scheme_object_file;
mod value;

use bdwgc_alloc::Allocator;
use error::Result;
use opcode::Op;
use scheme_object_file::SchemeObjectFile;
use std::cell::Cell;
use value::{Value, DYNENV_TAG};

#[global_allocator]
static GLOBAL_ALLOCATOR: Allocator = Allocator;

fn main() -> Result<()> {
    unsafe { Allocator::initialize() }

    let sco = SchemeObjectFile::from_file("../test1.sco")?;

    println!("{:#?}", sco);

    println!("{:?}", &sco.bytecode[..10]);
    println!("{:?}", &sco.bytecode[sco.entry_points[0]]);

    let mut vm = VirtualMachine::from_sco(sco);

    unsafe {
        println!("Result: {:?}", vm.run());
    }

    Ok(())
}

pub struct VirtualMachine {
    bytecode: Vec<u8>,
    pc: CodePointer,
    val: Value,
    fun: Value,
    arg1: Value,
    arg2: Value,
    env: &'static ActivationFrame,
    constants: Vec<Value>,
    globals: Vec<Value>,
    stack: Vec<Value>,
}

impl VirtualMachine {
    pub fn from_sco(sco: SchemeObjectFile) -> Self {
        VirtualMachine {
            val: Value::uninitialized(),
            fun: Value::uninitialized(),
            arg1: Value::uninitialized(),
            arg2: Value::uninitialized(),
            env: ActivationFrame::allocate(0),
            constants: sco.constants,
            globals: vec![Value::uninitialized(); sco.global_vars.len()],
            stack: vec![Value::Pointer(&sco.bytecode[1])], // pc for FINISH
            pc: CodePointer::new_unchecked(&sco.bytecode[sco.entry_points[0]]),
            bytecode: sco.bytecode,
        }
    }

    pub unsafe fn run(&mut self) -> Value {
        loop {
            match dbg!(self.pc.fetch_instruction()) {
                Op::Nop => {}
                Op::ShallowArgumentRef0 => self.val = *self.env.argument(0),
                Op::ShallowArgumentRef1 => self.val = *self.env.argument(1),
                Op::ShallowArgumentRef2 => self.val = *self.env.argument(2),
                Op::ShallowArgumentRef3 => self.val = *self.env.argument(3),
                Op::ShallowArgumentRef => {
                    let idx = self.pc.fetch_byte();
                    self.val = *self.env.argument(idx as usize);
                }
                Op::DeepArgumentRef => {
                    let i = self.pc.fetch_byte();
                    let j = self.pc.fetch_byte();
                    self.val = *self.env.deep_fetch(i as usize, j as usize);
                }
                Op::GlobalRef => {
                    let i = self.pc.fetch_byte();
                    self.val = *self.global_fetch(i as usize);
                }
                Op::CheckedGlobalRef => {
                    let i = self.pc.fetch_byte();
                    self.val = *self.global_fetch(i as usize);
                    if self.val.is_uninitialized() {
                        self.signal_exception("Uninitialized global variable")
                    }
                }
                Op::Constant => {
                    let i = self.pc.fetch_byte();
                    self.val = *self.quotation_fetch(i as usize);
                }

                Op::Finish => return self.val,

                Op::SetGlobal => {
                    let i = self.pc.fetch_byte();
                    self.global_update(i as usize, self.val);
                }

                Op::ShortGoto => {
                    let offset = self.pc.fetch_byte();
                    unsafe {
                        self.pc.jump(offset as isize);
                    }
                }
                Op::ShortJumpFalse => {
                    let offset = self.pc.fetch_byte();
                    if self.val.is_false() {
                        self.pc.jump(offset as isize);
                    }
                }

                Op::ExtendEnv => self.env = &self.val.as_frame().unwrap().extend(self.env),
                Op::UnlinkEnv => self.env = self.env.next().unwrap(),
                Op::PushValue => self.stack.push(self.val),
                Op::PopArg1 => self.arg1 = self.stack.pop().unwrap(),
                Op::PopArg2 => self.arg2 = self.stack.pop().unwrap(),
                Op::PreserveEnv => self.stack.push(Value::Frame(self.env)),
                Op::RestoreEnv => self.env = self.stack.pop().unwrap().as_frame().unwrap(),
                Op::PopFunction => self.fun = self.stack.pop().unwrap(),
                Op::CreateClosure => {
                    let offset = self.pc.fetch_byte();
                    self.val = Value::Closure(Closure::allocate(
                        self.pc.offset(offset as isize),
                        &self.env,
                    ));
                }
                Op::Return => self.pc = self.stack.pop().unwrap().into(),

                Op::FunctionInvoke => self.invoke(self.fun, false),
                Op::FunctionGoto => self.invoke(self.fun, true),

                Op::AllocateFrame1 => self.val = Value::Frame(ActivationFrame::allocate(1)),
                Op::AllocateFrame2 => self.val = Value::Frame(ActivationFrame::allocate(2)),
                Op::AllocateFrame3 => self.val = Value::Frame(ActivationFrame::allocate(3)),
                Op::AllocateFrame4 => self.val = Value::Frame(ActivationFrame::allocate(4)),
                Op::AllocateFrame5 => self.val = Value::Frame(ActivationFrame::allocate(5)),
                Op::AllocateFrame => {
                    let size = self.pc.fetch_byte();
                    self.val = Value::Frame(ActivationFrame::allocate(size as usize));
                }

                Op::PopFrame0 => self
                    .val
                    .as_frame()
                    .unwrap()
                    .set_argument(0, self.stack.pop().unwrap()),
                Op::PopFrame1 => self
                    .val
                    .as_frame()
                    .unwrap()
                    .set_argument(1, self.stack.pop().unwrap()),
                Op::PopFrame2 => self
                    .val
                    .as_frame()
                    .unwrap()
                    .set_argument(2, self.stack.pop().unwrap()),
                Op::PopFrame3 => self
                    .val
                    .as_frame()
                    .unwrap()
                    .set_argument(3, self.stack.pop().unwrap()),
                Op::PopFrame => {
                    let rank = self.pc.fetch_byte();
                    self.val
                        .as_frame()
                        .unwrap()
                        .set_argument(rank as usize, self.stack.pop().unwrap())
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
                    self.val = Value::int(val as i64);
                }
                Op::ConstMinus1 => self.val = Value::Int(-1),
                Op::Const0 => self.val = Value::Int(0),
                Op::Const1 => self.val = Value::Int(1),
                Op::Const2 => self.val = Value::Int(2),
                Op::Const4 => self.val = Value::Int(4),

                Op::Call2Cons => {
                    self.val = Value::cons(self.arg1, self.val);
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
        }
    }

    fn invoke(&mut self, f: Value, tail: bool) {
        match f {
            Value::Closure(cls) => {
                if !tail {
                    self.stack.push(self.pc.into());
                }
                self.env = cls.closed_environment;
                self.pc = cls.code;
            }
            _ => self.signal_exception(format!("Not a function {:?}", f)),
        }
    }

    fn signal_exception<T: ToString>(&self, msg: T) {
        unimplemented!("{}", msg.to_string())
    }

    fn global_fetch(&self, idx: usize) -> &Value {
        &self.globals[idx]
    }

    fn quotation_fetch(&self, idx: usize) -> &Value {
        &self.constants[idx]
    }

    fn global_update(&mut self, idx: usize, value: Value) {
        self.globals[idx] = value;
    }

    fn push_dynamic_binding(&mut self, index: usize, value: Value) {
        self.stack
            .push(Value::int(self.search_dynenv_index() as i64));
        self.stack.push(value);
        self.stack.push(Value::int(index as i64));
        self.stack.push(DYNENV_TAG);
    }

    fn pop_dynamic_binding(&mut self) {
        self.stack.pop();
        self.stack.pop();
        self.stack.pop();
        self.stack.pop();
    }

    fn find_dynamic_value(&self, index: usize) -> Value {
        let index = Value::Int(index as i64);
        let mut idx = self.search_dynenv_index();
        loop {
            if idx == 0 {
                return Value::uninitialized();
            }
            if self.stack[idx].eqv(&index) {
                return self.stack[idx - 1];
            }
            idx = self.stack[idx - 2].as_usize().unwrap();
        }
    }

    fn search_dynenv_index(&self) -> usize {
        let mut idx = self.stack.len();
        loop {
            if idx == 0 {
                return 0;
            }
            idx -= 1;
            if self.stack[idx].eqv(&DYNENV_TAG) {
                return idx - 1;
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct CodePointer {
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
        unsafe {
            let b = *self.ptr;
            self.ptr = self.ptr.offset(1);
            b
        }
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

impl From<CodePointer> for Value {
    fn from(pc: CodePointer) -> Self {
        Value::Pointer(pc.ptr)
    }
}

impl From<Value> for CodePointer {
    fn from(v: Value) -> Self {
        CodePointer {
            ptr: v.as_pointer().unwrap(),
        }
    }
}

#[derive(Debug)]
pub struct ActivationFrame {
    next: Cell<Option<&'static ActivationFrame>>,
    slots: Box<[Value]>,
}

impl ActivationFrame {
    fn new(size: usize) -> Self {
        ActivationFrame {
            next: Cell::new(None),
            slots: vec![Value::uninitialized(); size].into_boxed_slice(),
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

    fn argument(&self, idx: usize) -> &Value {
        &self.slots[idx]
    }

    fn set_argument(&self, idx: usize, value: Value) {
        unsafe {
            *(&self.slots[idx] as *const _ as *mut _) = value;
        }
    }

    fn extend(&self, env: &'static ActivationFrame) -> &Self {
        self.next.set(Some(env));
        self
    }

    fn deep_fetch(&self, i: usize, j: usize) -> &Value {
        let mut env = self;
        for _ in 0..i {
            env = env.next().unwrap();
        }
        env.argument(j)
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
