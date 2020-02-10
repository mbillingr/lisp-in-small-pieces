use crate::error::{Result, RuntimeError, TypeError};
use crate::objectify::Translate;
use crate::primitive::Arity;
use crate::scm::Scm;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::syntax::library::LibraryExportSpec;
use crate::syntax::variable::GlobalPlaceholder;
use crate::syntax::Variable;
use crate::utils::Named;
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug, Clone)]
pub struct CodeObject {
    source: SourceLocation,
    arity: Arity,
    constants: Box<[Scm]>,
    ops: Box<[Op]>,
}

#[derive(Debug)]
pub struct LibraryObject {
    code_object: CodeObject,
    global_symbols: Vec<Symbol>,
    exports: Vec<LibraryExportSpec>,
}

impl PartialEq for CodeObject {
    fn eq(&self, _other: &Self) -> bool {
        //self.ops == other.ops && self.constants == other.constants
        false
    }
}

#[derive(Debug)]
pub struct Closure {
    pub code: &'static CodeObject,
    pub free_vars: Box<[Scm]>,
}

#[derive(Copy, Clone, PartialEq)]
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

    Import,
    InitLibrary,

    // Intrinsics
    Cons,
    Car,
    Cdr,
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

impl LibraryObject {
    pub fn new(
        source: SourceLocation,
        code: impl Into<Box<[Op]>>,
        constants: impl Into<Box<[Scm]>>,
        global_symbols: impl Into<Vec<Symbol>>,
        exports: Vec<LibraryExportSpec>,
    ) -> Self {
        LibraryObject {
            code_object: CodeObject {
                source,
                arity: Arity::Exact(0),
                constants: constants.into(),
                ops: code.into(),
            },
            global_symbols: global_symbols.into(),
            exports,
        }
    }
}

pub type Library = HashMap<Symbol, Scm>;

pub struct VirtualMachine {
    pub trans: Translate,
    globals: Vec<(Scm, Symbol)>,
    libraries: HashMap<&'static str, Library>,
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
    pub fn new(trans: Translate, globals: Vec<(Scm, Symbol)>) -> Self {
        VirtualMachine {
            trans,
            globals,
            libraries: HashMap::new(),
            value_stack: vec![],
            call_stack: vec![],
        }
    }

    pub fn globals(&self) -> &[(Scm, Symbol)] {
        &self.globals
    }

    pub fn synchronize_globals(&mut self) {
        self.globals.resize(
            self.trans.env.max_global_idx(),
            (Scm::Uninitialized, Symbol::new("n/a")),
        );
        for (idx, gvar) in self.trans.env.enumerate_globals() {
            self.globals[idx].1 = gvar.name();
        }
    }

    pub fn add_library(&mut self, name: &'static str, library: Library) {
        self.libraries.insert(name, library);
    }

    pub fn invoke(&mut self, func: Scm, args: &[Scm]) -> Result<Scm> {
        match func {
            Scm::Closure(cls) => self.eval(cls, args),
            Scm::Primitive(pri) => (pri.func())(args, self),
            Scm::Continuation(_) => unimplemented!(),
            _ => Err(TypeError::NotCallable.into()),
        }
    }

    pub fn eval(&mut self, mut cls: &'static Closure, args: &[Scm]) -> Result<Scm> {
        self.value_stack.extend_from_slice(args);

        let mut frame_offset =
            self.value_stack.len() - self.convert_args(cls.code.arity, args.len());

        let mut val = Scm::Undefined;

        let mut ip: isize = 0;

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
                Op::PredefRef(idx) => val = self.globals[idx].0,
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
                    val = Scm::Undefined;
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
                    }
                    Scm::Primitive(func) => {
                        let n = self.value_stack.len() - nargs;
                        let args = self.value_stack[n..].to_vec();
                        self.value_stack.truncate(n);
                        val = func.invoke(&args, self)?;
                    }
                    Scm::Continuation(_cnt) => {
                        //self.value_stack = cnt.stack().clone();
                        unimplemented!()
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
                    }
                    Scm::Primitive(func) => {
                        let n = self.value_stack.len() - nargs;
                        let args = self.value_stack[n..].to_vec();
                        self.value_stack.truncate(n);
                        val = func.invoke(&args, self)?;

                        self.value_stack.truncate(frame_offset);
                        let data = self.call_stack.pop().expect("call-stack underflow");
                        frame_offset = data.0;
                        ip = data.1;
                        cls = data.2;
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
                Op::InitLibrary => {
                    let libname = val.as_string()?;
                    if !self.libraries.contains_key(libname) {
                        //println!("initializing {}", libname);

                        let global_offset = self.globals.len();
                        let mut trans = self.trans.same_but_empty();
                        let lib = crate::language::scheme::build_library(
                            &mut trans,
                            Path::new(libname),
                            global_offset,
                        )?;

                        self.trans.env.extend_global(
                            trans
                                .env
                                .globals()
                                .map(|_| Variable::GlobalPlaceholder(GlobalPlaceholder)),
                        );

                        self.synchronize_globals();
                        let code = Box::leak(Box::new(lib.code_object.clone()));
                        let closure = Closure::simple(code);
                        let closure = Box::leak(Box::new(closure));
                        self.eval(closure, &[])?;

                        let exports = lib
                            .exports
                            .iter()
                            .map(|spec| {
                                let idx = lib
                                    .global_symbols
                                    .iter()
                                    .position(|&n| n == spec.internal_name())
                                    .unwrap()
                                    + global_offset;
                                (spec.exported_name(), self.globals[idx].0)
                            })
                            .collect();

                        self.libraries.insert(libname, exports);

                        //unimplemented!()
                    }
                }
                Op::Import => {
                    // intended use:
                    //   (constant l)   ; load library name
                    //   (push-val)
                    //
                    //   (constant c)   ; load identifier name
                    //   (import)       ; import value of identifier from library l
                    //   (global-def g) ; store into global variable
                    //
                    //   (constant d)   ; load identifier name
                    //   (import)       ; import value of identifier from library l
                    //   (global-def h) ; store into global variable
                    //
                    //   (drop 1)
                    // ; The import instruction leaves the library name on the stack.
                    let identifier = val.as_symbol()?;
                    let libname = self.peek_value()?.as_string()?;
                    if !self.libraries.contains_key(&libname) {
                        unimplemented!()
                    }
                    match self.libraries[&libname].get(&identifier) {
                        Some(v) => val = *v,
                        _ => panic!("Invalid import"),
                    }
                }
                Op::Cons => {
                    let car = self.pop_value()?;
                    val = Scm::cons(car, val);
                }
                Op::Car => val = val.car()?,
                Op::Cdr => val = val.cdr()?,
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

    fn peek_value(&mut self) -> Result<Scm> {
        self.value_stack
            .last()
            .copied()
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
            Op::InitLibrary => write!(f, "(init-library)"),
            Op::Import => write!(f, "(import)"),
            Op::Cons => write!(f, "(cons)"),
            Op::Car => write!(f, "(car)"),
            Op::Cdr => write!(f, "(cdr)"),
        }
    }
}
