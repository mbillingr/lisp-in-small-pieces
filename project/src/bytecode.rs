use crate::ast_transform::generate_bytecode::{compile_library, GlobalAllocator};
use crate::continuation::{Continuation, ExitProcedure};
use crate::error::{Result, RuntimeError, TypeError};
use crate::objectify::Translate;
use crate::primitive::Arity;
use crate::scm::Scm;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::syntax::library::LibraryExportSpec;
use crate::syntax::Library;
use std::collections::HashSet;
use std::convert::TryInto;
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
    GlobalSet(usize),
    GlobalDef(usize),

    Boxify(usize),
    BoxSet,
    BoxGet,

    PushVal,

    JumpFalse(isize),
    Jump(isize),

    MakeClosure(usize, &'static CodeObject),
    PushCC(isize),
    PushEP(isize),
    PopEP,

    Call(usize),
    TailCall(usize),
    CallN,
    TailCallN,
    Return,
    Halt,
    PreApply(usize),

    Drop(usize),

    InitLibrary,

    // Intrinsics
    Cons,
    Car,
    Cdr,

    // Stack Operations
    Nip,
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

    pub fn invoke(&'static self, nargs: usize, vm: &mut VirtualMachine) {
        let n_locals = vm.convert_args(self.code.arity, nargs);
        vm.push_frame();
        vm.frame_offset = vm.value_stack.len() - n_locals;
        vm.ip = 0;
        vm.cls = self;
    }

    pub fn invoke_tail(&'static self, nargs: usize, vm: &mut VirtualMachine) {
        let n_locals = vm.convert_args(self.code.arity, nargs);

        let new_frame_offset = vm.value_stack.len() - n_locals;
        vm.value_stack
            .copy_within(new_frame_offset.., vm.frame_offset);
        vm.value_stack.truncate(vm.frame_offset + n_locals);

        vm.ip = 0;
        vm.cls = self;
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

pub struct VirtualMachine {
    pub trans: Translate,
    pub ga: GlobalAllocator,
    globals: Vec<Scm>,
    libraries: HashSet<&'static str>,
    pub value_stack: Vec<Scm>,
    pub call_stack: Vec<CallstackItem>,
    ip: isize,
    cls: &'static Closure,
    frame_offset: usize,
    pub val: Scm,
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
    pub fn new(trans: Translate) -> Self {
        VirtualMachine {
            trans,
            ga: GlobalAllocator::new(),
            globals: vec![],
            libraries: HashSet::new(),
            value_stack: vec![],
            call_stack: vec![],
            ip: 0,
            cls: HALT.with(|x| *x),
            frame_offset: 0,
            val: Scm::Undefined,
        }
    }

    pub fn globals(&self) -> &[Scm] {
        &self.globals
    }

    pub fn synchronize_globals(&mut self) {
        self.globals.resize(self.ga.n_vars(), Scm::Uninitialized);
    }

    pub fn add_library(&mut self, name: &'static str, library: &Library) {
        for var in library.env.globals() {
            let idx = self.ga.get_idx(&var);
            self.set_global(idx, var.value().as_scm());
        }

        self.libraries.insert(name);
    }

    pub fn set_global(&mut self, idx: usize, value: Scm) {
        if idx >= self.globals.len() {
            self.globals.resize(idx + 8, Scm::Uninitialized);
        }
        self.globals[idx] = value;
    }

    pub fn invoke(&mut self, func: Scm, args: &[Scm]) -> Result<Scm> {
        match func {
            Scm::Closure(cls) => self.eval(cls, args),
            Scm::Primitive(pri) => (pri.func())(args, self),
            Scm::Continuation(_) => unimplemented!(),
            f => Err(TypeError::NotCallable(f).into()),
        }
    }

    pub fn eval(&mut self, cls: &'static Closure, args: &[Scm]) -> Result<Scm> {
        // save old state
        self.push_frame();

        // insert intermediate state that halts the VM,
        // so that the we exit rather than continue executing the old state.
        self.ip = 0;
        self.cls = HALT.with(|x| *x);
        self.frame_offset = 0;
        self.push_frame();

        self.value_stack.extend_from_slice(args);

        self.frame_offset = self.value_stack.len() - self.convert_args(cls.code.arity, args.len());
        self.val = Scm::Undefined;
        self.ip = 0;
        self.cls = cls;

        let result = self.run();

        // restore old state
        self.pop_state()?;

        result
    }

    fn run(&mut self) -> Result<Scm> {
        loop {
            let op = &self.cls.code.ops[self.ip as usize];
            //println!("{:?}", op);
            self.ip += 1;
            match *op {
                Op::Constant(idx) => self.val = self.cls.code.constants[idx],
                Op::LocalRef(idx) => self.val = self.ref_value(idx + self.frame_offset)?,
                Op::GlobalRef(idx) => {
                    self.val = self.globals[idx];
                    if self.val.is_uninitialized() {
                        return Err(RuntimeError::UndefinedGlobal(
                            self.ga.find_var(idx).full_name(),
                        )
                        .into());
                    }
                    if self.val.is_cell() {
                        self.val = self.val.get().unwrap();
                    }
                }
                Op::FreeRef(idx) => self.val = self.cls.free_vars[idx],
                Op::GlobalSet(idx) => {
                    if self.globals[idx].is_uninitialized() {
                        return Err(RuntimeError::UndefinedGlobal(
                            self.ga.find_var(idx).full_name(),
                        )
                        .into());
                    }
                    let glob = &mut self.globals[idx];
                    if glob.is_cell() {
                        glob.set(self.val).unwrap();
                    } else {
                        *glob = self.val;
                    }
                }
                Op::GlobalDef(idx) => {
                    self.globals[idx] = self.val;
                    self.val = Scm::Undefined;
                }
                Op::Boxify(idx) => {
                    let x = self.ref_value(idx + self.frame_offset)?;
                    self.push_value(Scm::boxed(x));
                }
                Op::BoxSet => self
                    .pop_value()?
                    .set(self.val)
                    .expect("setting unboxed value"),
                Op::BoxGet => self.val = self.val.get().expect("getting unboxed value"),
                Op::PushVal => self.push_value(self.val),
                Op::Jump(delta) => self.ip += delta,
                Op::JumpFalse(delta) => {
                    if self.val.is_false() {
                        self.ip += delta
                    }
                }
                Op::MakeClosure(n_free, func) => {
                    let mut vars = Vec::with_capacity(n_free);
                    for _ in 0..n_free {
                        vars.push(self.pop_value()?);
                    }
                    self.val = Scm::closure(func, vars);
                }
                Op::PushCC(offset) => {
                    let cc = Continuation::new(offset, self);
                    let cc = Box::leak(Box::new(cc));
                    self.value_stack.push(Scm::Continuation(cc));
                }
                Op::PushEP(offset) => {
                    let ep = ExitProcedure::new(offset, self);
                    let ep = Box::leak(Box::new(ep));
                    self.value_stack.push(Scm::ExitProc(ep));

                    // the exit procedure is valid until this marker is popped from the stack
                    self.call_stack.push(CallstackItem::ExitProc(ep));
                }
                Op::PopEP => {
                    if let Some(CallstackItem::ExitProc(_)) = self.call_stack.pop() {
                    } else {
                        panic!("could not pop exit procedure")
                    }
                }
                Op::Call(nargs) => {
                    let val = self.val;
                    val.invoke(nargs, self)?;
                }
                Op::TailCall(nargs) => {
                    let val = self.val;
                    val.invoke_tail(nargs, self)?;
                }
                Op::CallN => {
                    let nargs = self.pop_value()?.try_into()?;
                    let val = self.val;
                    val.invoke(nargs, self)?;
                }
                Op::TailCallN => {
                    let nargs = self.pop_value()?.try_into()?;
                    let val = self.val;
                    val.invoke_tail(nargs, self)?;
                }
                Op::Return => self.do_return()?,
                Op::Halt => return Ok(self.val),
                Op::PreApply(nargs) => {
                    // value-stack: func a1 a2 a3 ... aa
                    let n = self.value_stack.len();
                    let func = self.value_stack[n + 1 - nargs];

                    let mut n_list = 0;
                    let mut list = self.val;
                    while list.is_pair() {
                        n_list += 1;
                        self.push_value(list.car().unwrap());
                        list = list.cdr().unwrap();
                    }

                    let n_args = nargs - 2 + n_list;
                    self.push_value(n_args.into());
                    self.val = func;
                }
                Op::Drop(n) => self.value_stack.truncate(self.value_stack.len() - n),
                Op::InitLibrary => {
                    let libname = self.val.as_string().unwrap();
                    self.init_library(libname)?;
                    self.val = Scm::Undefined;
                }
                Op::Cons => {
                    let car = self.pop_value()?;
                    self.val = Scm::cons(car, self.val);
                }
                Op::Car => self.val = self.val.car()?,
                Op::Cdr => self.val = self.val.cdr()?,

                Op::Nip => self.val = self.value_stack.swap_remove(self.value_stack.len() - 2),
            }
        }
    }

    pub fn do_return(&mut self) -> Result<()> {
        self.value_stack.truncate(self.frame_offset);
        self.pop_state()
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

    fn push_frame(&mut self) {
        self.call_stack
            .push(CallstackItem::Frame(self.current_frame()));
    }

    pub fn pop_state(&mut self) -> Result<()> {
        match self
            .call_stack
            .pop()
            .unwrap_or_else(|| panic!("call stack underflow"))
        {
            CallstackItem::Frame(frame) => Ok(self.set_frame(frame)),
            CallstackItem::ExitProc(_) => self.pop_state(),
        }
    }

    pub fn current_frame(&self) -> CallstackFrame {
        CallstackFrame {
            frame_offset: self.frame_offset,
            ip: self.ip,
            closure: self.cls,
        }
    }

    pub fn set_frame(&mut self, frame: CallstackFrame) {
        self.frame_offset = frame.frame_offset;
        self.ip = frame.ip;
        self.cls = frame.closure;
    }

    pub fn pop_value(&mut self) -> Result<Scm> {
        self.value_stack
            .pop()
            .ok_or_else(|| panic!("value stack underflow"))
        //.ok_or(RuntimeError::ValueStackUnderflow.into())
    }

    fn push_value(&mut self, value: Scm) {
        self.value_stack.push(value)
    }

    fn ref_value(&mut self, idx: usize) -> Result<Scm> {
        Ok(self.value_stack[idx])
    }

    pub fn init_library(&mut self, libname: &'static str) -> Result<()> {
        if !self.libraries.contains(libname) {
            //println!("initializing {}", libname);
            self.libraries.insert(libname);

            let mut trans = self.trans.same_but_empty();
            let lib = crate::language::scheme::load_library(&mut trans, Path::new(libname))?;

            for set in &lib.imports.import_sets {
                self.init_library(set.library_symb.as_str())?;
            }

            //println!("compiling {}", libname);
            let lib_obj = compile_library(&lib, &trans, &mut self.ga)?;

            self.synchronize_globals();
            let code = Box::leak(Box::new(lib_obj.code_object.clone()));
            let closure = Closure::simple(code);
            let closure = Box::leak(Box::new(closure));
            self.eval(closure, &[])?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Op::Constant(c) => write!(f, "(constant {})", c),
            Op::LocalRef(r) => write!(f, "(local-ref {})", r),
            Op::FreeRef(r) => write!(f, "(free-ref {})", r),
            Op::GlobalRef(r) => write!(f, "(global-ref {})", r),
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
            Op::PushCC(o) => write!(f, "(push/cc {})", o),
            Op::PushEP(o) => write!(f, "(push/ep {})", o),
            Op::PopEP => write!(f, "(pop/ep)"),
            Op::Call(n_args) => write!(f, "(call {})", n_args),
            Op::TailCall(n_args) => write!(f, "(tail-call {})", n_args),
            Op::CallN => write!(f, "(call-n)"),
            Op::TailCallN => write!(f, "(tail-call-n)"),
            Op::Return => write!(f, "(return)"),
            Op::Halt => write!(f, "(halt)"),
            Op::PreApply(n_args) => write!(f, "(prepare-apply {})", n_args),
            Op::Drop(n) => write!(f, "(drop {})", n),
            Op::InitLibrary => write!(f, "(init-library)"),
            Op::Cons => write!(f, "(cons)"),
            Op::Car => write!(f, "(car)"),
            Op::Cdr => write!(f, "(cdr)"),
            Op::Nip => write!(f, "(nip)"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum CallstackItem {
    Frame(CallstackFrame),
    ExitProc(&'static ExitProcedure),
}

#[derive(Debug, Copy, Clone)]
pub struct CallstackFrame {
    ip: isize,
    frame_offset: usize,
    closure: &'static Closure,
}

impl CallstackFrame {
    pub fn with_ip_offset(self, offset: isize) -> Self {
        CallstackFrame {
            ip: self.ip + offset,
            ..self
        }
    }
}
