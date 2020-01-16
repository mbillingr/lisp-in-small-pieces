use crate::description::Arity;
use crate::env::Environment;
use crate::error::{Result, RuntimeError, TypeError};
use crate::library::ExportItem;
use crate::library::Library;
use crate::scm::Scm;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::syntax::GlobalVariable;
use crate::utils::Named;
use std::rc::Rc;

#[derive(Debug)]
pub struct CodeObject {
    source: SourceLocation,
    arity: Arity,
    constants: Box<[Scm]>,
    ops: Box<[Op]>,
}

#[derive(Debug)]
pub struct Closure {
    pub code: &'static CodeObject,
    pub free_vars: Box<[Scm]>,
}

#[derive(Copy, Clone)]
pub enum Op {
    Constant(usize),
    LocalRef(usize),
    FreeRef(usize),
    GlobalRef(usize),
    PredefRef(usize),
    GlobalSet(usize),
    GlobalDef(usize),

    Boxify(usize),
    BoxSet,
    BoxGet,

    PushVal,

    JumpFalse(isize),
    Jump(isize),

    MakeClosure(usize, &'static CodeObject),

    Call(usize),
    TailCall(usize),
    Return,
    Halt,

    Drop(usize),

    Import(usize),
}

impl Op {
    pub fn is_terminal(&self) -> bool {
        match self {
            Op::Jump(_) | Op::TailCall(_) | Op::Return | Op::Halt => true,
            _ => false,
        }
    }
}

impl Closure {
    pub fn new(code: &'static CodeObject, free_vars: Box<[Scm]>) -> Self {
        Closure { code, free_vars }
    }
    pub fn simple(code: &'static CodeObject) -> Self {
        Closure {
            code,
            free_vars: Box::new([]),
        }
    }
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
            ops: code.into(),
        }
    }
}

pub struct VirtualMachine {
    globals: Vec<(Scm, Symbol)>,
    predef: Vec<Scm>,
    libraries: Vec<Rc<Library>>,
    value_stack: Vec<Scm>,
    call_stack: Vec<(usize, isize, &'static Closure)>,
}

thread_local! {
    static HALT: &'static Closure = Box::leak(Box::new(Closure{
        code: Box::leak(Box::new(CodeObject {
            arity: Arity::Exact(0),
            source: SourceLocation::NoSource,
            constants: Box::new([]),
            ops: Box::new([Op::Halt]),
        })),
        free_vars: Box::new([]),
        }));
}

impl VirtualMachine {
    pub fn new(globals: Vec<(Scm, Symbol)>, predef: Vec<Scm>) -> Self {
        VirtualMachine {
            globals,
            predef,
            libraries: vec![],
            value_stack: vec![],
            call_stack: vec![],
        }
    }

    pub fn globals(&self) -> &[(Scm, Symbol)] {
        &self.globals
    }

    pub fn add_globals(&mut self, env: &Environment<GlobalVariable>) {
        for i in self.globals.len()..env.len() {
            self.globals.push((Scm::uninitialized(), env.at(i).name()));
        }
    }

    pub fn add_library(&mut self, library: Rc<Library>) {
        self.libraries.push(library);
    }

    /*pub fn resize_globals(&mut self, n: usize) {
        self.globals.resize(n, Scm::uninitialized())
    }*/

    pub fn eval(&mut self, mut cls: &'static Closure) -> Result<Scm> {
        let mut val = Scm::Undefined;

        let mut ip: isize = 0;
        let mut frame_offset = 0;

        self.call_stack.push((0, 0, HALT.with(|x| *x)));

        loop {
            let op = &cls.code.ops[ip as usize];
            ip += 1;
            match *op {
                Op::Constant(idx) => val = cls.code.constants[idx],
                Op::LocalRef(idx) => val = self.ref_value(idx + frame_offset)?,
                Op::GlobalRef(idx) => {
                    val = self.globals[idx].0;
                    if val.is_uninitialized() {
                        return Err(
                            RuntimeError::UndefinedGlobal(self.globals[idx].1.clone()).into()
                        );
                    }
                    if val.is_cell() {
                        val = val.get().unwrap();
                    }
                }
                Op::FreeRef(idx) => val = cls.free_vars[idx],
                Op::PredefRef(idx) => val = self.predef[idx],
                Op::GlobalSet(idx) => {
                    if self.globals[idx].0.is_uninitialized() {
                        return Err(RuntimeError::UndefinedGlobal(self.globals[idx].1).into());
                    }
                    let glob = &mut self.globals[idx].0;
                    if glob.is_cell() {
                        glob.set(val).unwrap();
                    } else {
                        *glob = val;
                    }
                }
                Op::GlobalDef(idx) => {
                    self.globals[idx].0 = val;
                }
                Op::Boxify(idx) => {
                    let x = self.ref_value(idx + frame_offset)?;
                    self.push_value(Scm::boxed(x));
                }
                Op::BoxSet => {
                    let boxed = self.pop_value()?;
                    boxed.set(val).expect("setting unboxed value");
                }
                Op::BoxGet => {
                    val = val.get().expect("getting unboxed value");
                }
                Op::PushVal => self.push_value(val),
                Op::Jump(delta) => ip += delta,
                Op::JumpFalse(delta) => {
                    if val.is_false() {
                        ip += delta
                    }
                }
                Op::MakeClosure(n_free, func) => {
                    let mut vars = Vec::with_capacity(n_free);
                    for _ in 0..n_free {
                        vars.push(self.pop_value()?);
                    }
                    val = Scm::closure(func, vars);
                }
                Op::Call(nargs) => match val {
                    Scm::Closure(callee) => {
                        let n_locals = self.convert_args(callee.code.arity, nargs);

                        self.call_stack.push((frame_offset, ip, cls));
                        frame_offset = self.value_stack.len() - n_locals;

                        ip = 0;
                        cls = callee;
                        //free = fv;
                    }
                    Scm::Primitive(func) => {
                        let n = self.value_stack.len() - nargs;
                        val = func.invoke(&self.value_stack[n..])?;
                        self.value_stack.truncate(n);
                    }
                    _ => return Err(TypeError::NotCallable.into()),
                },
                Op::TailCall(nargs) => match val {
                    Scm::Closure(callee) => {
                        let n_locals = self.convert_args(callee.code.arity, nargs);

                        let new_frame_offset = self.value_stack.len() - n_locals;
                        self.value_stack
                            .copy_within(new_frame_offset.., frame_offset);
                        self.value_stack.truncate(frame_offset + n_locals);

                        ip = 0;
                        cls = callee;
                        //free = fv;
                    }
                    _ => return Err(TypeError::NotCallable.into()),
                },
                Op::Return => {
                    self.value_stack.truncate(frame_offset);
                    let data = self.call_stack.pop().expect("call-stack underflow");
                    frame_offset = data.0;
                    ip = data.1;
                    cls = data.2;
                }
                Op::Halt => return Ok(val),
                Op::Drop(n) => {
                    self.value_stack.truncate(self.value_stack.len() - n);
                }
                Op::Import(l) => {
                    // intended use:
                    //   (constant c)   ; load identifier name
                    //   (import l)     ; import value of identifier from library l
                    //   (global-def g) ; store into global variable
                    let identifier = val.as_symbol()?;
                    match self.libraries[l].lookup(identifier) {
                        Some(ExportItem::Value(v)) => val = *v,
                        _ => panic!("Invalid import"),
                    }
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
                for _ in 0..n_passed - n as usize {
                    let b = self.pop_value().unwrap();
                    let a = self.pop_value().unwrap();
                    self.push_value(Scm::cons(a, b));
                }
                1 + n as usize
            }
        }
    }

    fn pop_value(&mut self) -> Result<Scm> {
        self.value_stack
            .pop()
            .ok_or(RuntimeError::ValueStackUnderflow.into())
    }

    fn push_value(&mut self, value: Scm) {
        self.value_stack.push(value)
    }

    fn ref_value(&mut self, idx: usize) -> Result<Scm> {
        Ok(self.value_stack[idx])
    }
}

impl std::fmt::Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Op::Constant(c) => write!(f, "(constant {})", c),
            Op::LocalRef(r) => write!(f, "(local-ref {})", r),
            Op::FreeRef(r) => write!(f, "(free-ref {})", r),
            Op::GlobalRef(r) => write!(f, "(global-ref {})", r),
            Op::PredefRef(r) => write!(f, "(predef-ref {})", r),
            Op::GlobalSet(r) => write!(f, "(global-set {})", r),
            Op::GlobalDef(r) => write!(f, "(global-def {})", r),
            Op::Boxify(r) => write!(f, "(boxify {})", r),
            Op::BoxSet => write!(f, "(box-set)"),
            Op::BoxGet => write!(f, "(box-get)"),
            Op::PushVal => write!(f, "(push-val)"),
            Op::JumpFalse(loc) => write!(f, "(jump-false {})", loc),
            Op::Jump(loc) => write!(f, "(jump {})", loc),
            Op::MakeClosure(n_free, code) => {
                if f.alternate() {
                    write!(f, "(make-closure {} {:#?})", n_free, *code)
                } else {
                    write!(f, "(make-closure {} {:p})", n_free, *code)
                }
            }
            Op::Call(n_args) => write!(f, "(call {})", n_args),
            Op::TailCall(n_args) => write!(f, "(tail-call {})", n_args),
            Op::Return => write!(f, "(return)"),
            Op::Halt => write!(f, "(halt)"),
            Op::Drop(n) => write!(f, "(drop {})", n),
            Op::Import(l) => write!(f, "(import {})", l),
        }
    }
}
