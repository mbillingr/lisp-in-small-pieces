use crate::ast::Arity;
use crate::scm::Scm;
use crate::source::{Source, SourceLocation};
use lazy_static::lazy_static;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    ValueStackUnderflow,
    NotCallable,
}

#[derive(Debug)]
pub struct CodeObject {
    source: SourceLocation,
    arity: Arity,
    constants: Box<[Scm]>,
    code: Box<[Op]>,
}

#[derive(Debug, Copy, Clone)]
pub enum Op {
    Constant(usize),
    LocalRef(usize),
    FreeRef(usize),
    GlobalRef(usize),
    PredefRef(usize),
    GlobalSet(usize),

    Boxify(usize),
    BoxSet(usize),
    BoxGet(usize),

    PushVal,
    PopVal,

    JumpFalse(isize),
    Jump(isize),

    MakeClosure(&'static CodeObject, usize),

    Call(usize),
    TailCall(usize),
    CallPrimitive(usize),
    Return,
    Halt,

    Drop(usize),
}

impl CodeObject {
    pub fn new(
        arity: Arity,
        source: SourceLocation,
        code: impl Into<Box<[Op]>>,
        constants: impl Into<Box<[Scm]>>,
    ) -> Self {
        CodeObject {
            source,
            arity,
            constants: constants.into(),
            code: code.into(),
        }
    }
}

pub struct VirtualMachine {
    globals: Vec<Scm>,
    predef: Vec<Scm>,
    value_stack: Vec<Scm>,
    call_stack: Vec<(usize, isize, &'static CodeObject, &'static [Scm])>,
}

thread_local! {
    static HALT: &'static CodeObject = Box::leak(Box::new(CodeObject {
            arity: Arity::Exact(0),
            source: SourceLocation::NoSource,
            constants: Box::new([]),
            code: Box::new([Op::Halt]),
        }));
}

impl VirtualMachine {
    pub fn new(globals: Vec<Scm>, predef: Vec<Scm>) -> Self {
        VirtualMachine {
            globals,
            predef,
            value_stack: vec![],
            call_stack: vec![],
        }
    }

    pub fn resize_globals(&mut self, n: usize) {
        self.globals.resize(n, Scm::uninitialized())
    }

    pub fn eval(&mut self, mut code: &'static CodeObject) -> Result<Scm> {
        let mut val = Scm::Undefined;
        let mut free: &[Scm] = &[];

        let mut ip: isize = 0;
        let mut frame_offset = 0;

        self.call_stack.push((0, 0, HALT.with(|x| *x), &[]));

        loop {
            let op = &code.code[ip as usize];
            ip += 1;
            match *op {
                Op::Constant(idx) => val = code.constants[idx],
                Op::LocalRef(idx) => val = self.ref_value(idx + frame_offset)?,
                Op::GlobalRef(idx) => val = self.globals[idx],
                Op::FreeRef(idx) => unimplemented!(),
                Op::PredefRef(idx) => val = self.predef[idx],
                Op::GlobalSet(idx) => self.globals[idx] = val,
                Op::Boxify(idx) => {
                    let x = self.ref_value(idx + frame_offset)?;
                    self.push_value(Scm::boxed(x));
                }
                Op::BoxSet(idx) => {
                    let boxed = self.ref_value(idx + frame_offset)?;
                    boxed.set(val).expect("setting unboxed value");
                }
                Op::BoxGet(idx) => {
                    let boxed = self.ref_value(idx + frame_offset)?;
                    val = boxed.get().expect("getting unboxed value");
                }
                Op::PushVal => self.push_value(val),
                Op::PopVal => val = self.pop_value()?,
                Op::Jump(delta) => ip += delta,
                Op::JumpFalse(delta) => {
                    if val.is_false() {
                        ip += delta
                    }
                }
                Op::MakeClosure(func, n_free) => {
                    let mut vars = Vec::with_capacity(n_free);
                    for _ in 0..n_free {
                        vars.push(self.pop_value()?);
                    }
                    val = Scm::closure(func, vars);
                }
                Op::Call(nargs) => match val {
                    Scm::Closure(cc, fv) => {
                        let n_locals = self.convert_args(cc.arity, nargs);

                        self.call_stack.push((frame_offset, ip, code, free));
                        frame_offset = self.value_stack.len() - n_locals;

                        ip = 0;
                        code = cc;
                        free = fv;
                    }
                    Scm::Primitive(func) => {
                        let n = self.value_stack.len() - nargs;
                        val = func.invoke(&self.value_stack[n..]);
                        self.value_stack.truncate(n);
                    }
                    _ => return Err(Error::NotCallable),
                },
                Op::TailCall(nargs) => match val {
                    Scm::Closure(cc, fv) => {
                        let n_locals = self.convert_args(cc.arity, nargs);

                        let new_frame_offset = self.value_stack.len() - n_locals;
                        self.value_stack
                            .copy_within(new_frame_offset.., frame_offset);
                        self.value_stack.truncate(frame_offset + n_locals);

                        ip = 0;
                        code = cc;
                        free = fv;
                    }
                    _ => return Err(Error::NotCallable),
                },
                Op::CallPrimitive(arity) => unimplemented!(),
                Op::Return => {
                    self.value_stack.truncate(frame_offset);
                    let data = self.call_stack.pop().expect("call-stack underflow");
                    frame_offset = data.0;
                    ip = data.1;
                    code = data.2;
                    free = data.3;
                }
                Op::Halt => return Ok(val),
                Op::Drop(n) => {
                    self.value_stack.truncate(self.value_stack.len() - n);
                }
            }
        }
    }

    fn convert_args(&mut self, arity: Arity, n_passed: usize) -> usize {
        match arity {
            Arity::Exact(n) => {
                assert_eq!(n as usize, n_passed);
                n_passed
            }
            Arity::AtLeast(n) => {
                assert!(n_passed >= n as usize);
                self.push_value(Scm::nil());
                for _ in (0..n_passed - n as usize) {
                    let b = self.pop_value().unwrap();
                    let a = self.pop_value().unwrap();
                    self.push_value(Scm::cons(a, b));
                }
                1 + n as usize
            }
        }
    }

    fn pop_value(&mut self) -> Result<Scm> {
        self.value_stack.pop().ok_or(Error::ValueStackUnderflow)
    }

    fn push_value(&mut self, value: Scm) {
        self.value_stack.push(value)
    }

    fn ref_value(&mut self, idx: usize) -> Result<Scm> {
        Ok(self.value_stack[idx])
    }
}
