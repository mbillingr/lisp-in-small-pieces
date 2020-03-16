use crate::bytecode::{Closure, CodeObject, VirtualMachine};
use crate::continuation::{Continuation, ExitProcedure};
use crate::error::{Error, Result, TypeError};
use crate::ports::SchemePort;
use crate::primitive::RuntimePrimitive;
use crate::scm_write::{ScmDisplay, ScmWriteShared, ScmWriteSimple};
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::symbol::Symbol;
use std::any::Any;
use std::cell::Cell;
use std::convert::TryFrom;

#[derive(Copy, Clone)]
pub struct Scm<T: 'static = ()> {
    pub value: ScmValue<T>,
    pub meta_data: T,
}

#[derive(Copy, Clone)]
pub enum ScmValue<T: 'static = ()> {
    Undefined,
    Uninitialized,
    Nil,
    True,
    False,
    Eof,
    Int(i64),
    Float(f64),
    Char(char),
    Symbol(Symbol),
    String(&'static str),
    Vector(&'static [Cell<Scm<T>>]),
    Bytevector(&'static [u8]),

    Pair(&'static (Cell<Scm<T>>, Cell<Scm<T>>)),

    Closure(&'static Closure),
    Primitive(RuntimePrimitive),
    Continuation(&'static Continuation),
    ExitProc(&'static ExitProcedure),

    Port(&'static SchemePort),

    Cell(&'static Cell<Scm>),
    Rust(&'static Box<dyn Any>),
    Error(&'static Error),
}

impl<T: Default> From<ScmValue<T>> for Scm<T> {
    fn from(value: ScmValue<T>) -> Scm<T> {
        Scm {
            value,
            meta_data: Default::default(),
        }
    }
}

impl Scm {
    pub fn uninitialized() -> Self {
        ScmValue::Uninitialized.into()
    }
    pub fn undefined() -> Self {
        ScmValue::Undefined.into()
    }

    pub fn nil() -> Self {
        ScmValue::Nil.into()
    }

    pub fn bool(b: bool) -> Self {
        match b {
            true => ScmValue::True,
            false => ScmValue::False,
        }
        .into()
    }

    pub fn int(x: i64) -> Self {
        ScmValue::Int(x).into()
    }

    pub fn float(x: f64) -> Self {
        ScmValue::Float(x).into()
    }

    pub fn eof() -> Self {
        ScmValue::Eof.into()
    }

    pub fn boxed(x: Scm) -> Scm {
        ScmValue::Cell(Box::leak(Box::new(Cell::new(x)))).into()
    }

    pub fn cons(car: Scm, cdr: Scm) -> Scm {
        ScmValue::Pair(Box::leak(Box::new((Cell::new(car), Cell::new(cdr))))).into()
    }

    pub fn closure(func: &'static CodeObject, free_vars: impl Into<Box<[Scm]>>) -> Self {
        ScmValue::Closure(Box::leak(Box::new(Closure::new(func, free_vars.into())))).into()
    }

    pub fn symbol(s: impl Into<Symbol>) -> Self {
        ScmValue::Symbol(s.into()).into()
    }

    pub fn string(s: impl Into<Box<str>>) -> Self {
        ScmValue::String(Box::leak(s.into())).into()
    }

    pub fn error(e: impl Into<Error>) -> Self {
        ScmValue::Error(Box::leak(Box::new(e.into()))).into()
    }

    pub fn list<T, I>(items: T) -> Self
    where
        T: IntoIterator<Item = Scm, IntoIter = I>,
        I: DoubleEndedIterator<Item = Scm>,
    {
        let mut out = Scm::nil();
        for x in items.into_iter().rev() {
            out = Scm::cons(x, out);
        }
        out
    }

    pub fn vector(items: impl IntoIterator<Item = Scm>) -> Self {
        let v: Vec<Cell<Scm>> = items.into_iter().map(Cell::new).collect();
        let static_data = Box::leak(v.into_boxed_slice());
        ScmValue::Vector(static_data).into()
    }

    pub fn primitive(proc: RuntimePrimitive) -> Self {
        ScmValue::Primitive(proc).into()
    }

    pub fn continuation(c: &'static Continuation) -> Self {
        ScmValue::Continuation(c).into()
    }

    pub fn exit_proc(ep: &'static ExitProcedure) -> Self {
        ScmValue::ExitProc(ep).into()
    }

    pub fn rust_object<T: 'static>(obj: T) -> Self {
        let boxed: Box<dyn Any> = Box::new(obj);
        let obj = Box::leak(Box::new(boxed));
        ScmValue::Rust(obj).into()
    }

    pub fn is_undefined(&self) -> bool {
        match self.value {
            ScmValue::Undefined => true,
            _ => false,
        }
    }

    pub fn is_uninitialized(&self) -> bool {
        match self.value {
            ScmValue::Uninitialized => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self.value {
            ScmValue::Nil => true,
            _ => false,
        }
    }

    pub fn is_eof(&self) -> bool {
        match self.value {
            ScmValue::Eof => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self.value {
            ScmValue::True | ScmValue::False => true,
            _ => false,
        }
    }

    pub fn is_false(&self) -> bool {
        match self.value {
            ScmValue::False => true,
            _ => false,
        }
    }

    pub fn is_procedure(&self) -> bool {
        match self.value {
            ScmValue::Closure(_)
            | ScmValue::Primitive(_)
            | ScmValue::Continuation(_)
            | ScmValue::ExitProc(_) => true,
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self.value {
            ScmValue::Primitive(_) => true,
            _ => false,
        }
    }

    pub fn is_cell(&self) -> bool {
        match self.value {
            ScmValue::Cell(_) => true,
            _ => false,
        }
    }

    pub fn as_int(&self) -> Result<i64> {
        match self.value {
            ScmValue::Int(i) => Ok(i),
            _ => Err(TypeError::NoInt.into()),
        }
    }

    pub fn as_char(&self) -> Result<char> {
        match self.value {
            ScmValue::Char(ch) => Ok(ch),
            _ => Err(TypeError::NoChar(*self).into()),
        }
    }

    pub fn as_symbol(&self) -> Result<Symbol> {
        match self.value {
            ScmValue::Symbol(s) => Ok(s),
            _ => Err(TypeError::NoSymbol.into()),
        }
    }

    pub fn as_string(&self) -> Result<&'static str> {
        match self.value {
            ScmValue::String(s) => Ok(s),
            _ => Err(TypeError::NoString(*self).into()),
        }
    }

    pub fn is_rust_object(&self) -> bool {
        self.as_rust_object().is_ok()
    }

    pub fn as_rust_object(&self) -> Result<&'static dyn Any> {
        match self.value {
            ScmValue::Rust(o) => Ok(&**o),
            _ => Err(TypeError::NoRustObject(*self).into()),
        }
    }

    pub fn is_pair(&self) -> bool {
        match self.value {
            ScmValue::Pair(_) => true,
            _ => false,
        }
    }

    pub fn car(&self) -> Result<Scm> {
        match self.value {
            ScmValue::Pair(p) => Ok(p.0.get()),
            _ => Err(TypeError::NoPair(*self).into()),
        }
    }

    pub fn cdr(&self) -> Result<Scm> {
        match self.value {
            ScmValue::Pair(p) => Ok(p.1.get()),
            _ => Err(TypeError::NoPair(*self).into()),
        }
    }

    pub fn set_car(&self, x: Scm) -> Result<Scm> {
        match self.value {
            ScmValue::Pair(p) => {
                p.0.set(x);
                Ok(Scm::undefined())
            }
            _ => Err(TypeError::NoPair(*self).into()),
        }
    }

    pub fn set_cdr(&self, x: Scm) -> Result<Scm> {
        match self.value {
            ScmValue::Pair(p) => {
                p.1.set(x);
                Ok(Scm::undefined())
            }
            _ => Err(TypeError::NoPair(*self).into()),
        }
    }

    pub fn is_vector(&self) -> bool {
        self.as_vector().is_ok()
    }

    pub fn as_vector(&self) -> Result<&'static [Cell<Scm>]> {
        match self.value {
            ScmValue::Vector(v) => Ok(v),
            _ => Err(TypeError::NoVector.into()),
        }
    }

    pub fn vector_ref(&self, idx: usize) -> Result<Scm> {
        match self.value {
            ScmValue::Vector(v) => v
                .get(idx)
                .map(Cell::get)
                .ok_or(TypeError::OutOfBounds.into()),
            _ => Err(TypeError::NoVector.into()),
        }
    }

    pub fn vector_set(&self, idx: usize, val: Scm) -> Result<()> {
        match self.value {
            ScmValue::Vector(v) => match v.get(idx) {
                Some(c) => {
                    c.set(val);
                    Ok(())
                }
                None => Err(TypeError::OutOfBounds.into()),
            },
            _ => Err(TypeError::NoVector.into()),
        }
    }

    pub fn as_closure(&self) -> Result<&'static Closure> {
        match self.value {
            ScmValue::Closure(cls) => Ok(cls),
            _ => Err(TypeError::NoClosure.into()),
        }
    }

    pub fn as_port(&self) -> Result<&'static SchemePort> {
        match self.value {
            ScmValue::Port(p) => Ok(p),
            _ => Err(TypeError::NoPort(*self).into()),
        }
    }

    pub fn as_bytevec(&self) -> Result<&'static [u8]> {
        match self.value {
            ScmValue::Bytevector(v) => Ok(v),
            _ => Err(TypeError::NoBytevector(*self).into()),
        }
    }

    pub fn as_mut_bytevec(&self) -> Result<&'static mut [u8]> {
        match self.value {
            ScmValue::Bytevector(v) => unsafe {
                let ptr = v as *const _;
                let mut_ptr = ptr as *mut _;
                Ok(&mut *mut_ptr)
            },
            _ => Err(TypeError::NoBytevector(*self).into()),
        }
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.value.ptr_eq(&other.value)
    }

    pub fn equals(&self, other: &Self) -> bool {
        use ScmValue::*;
        match (self.value, other.value) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Vector(a), Vector(b)) => a.iter().zip(b).all(|(a, b)| a.get().equals(&b.get())),
            (Pair(a), Pair(b)) => a.0.get().equals(&b.0.get()) && a.1.get().equals(&b.1.get()),
            (Primitive(a), Primitive(b)) => a == b,
            (Cell(a), Cell(b)) => a.get().equals(&b.get()),
            (Error(a), Error(b)) => a == b,
            _ => false,
        }
    }

    pub fn num_eq(&self, other: &Self) -> bool {
        use ScmValue::*;
        match (self.value, other.value) {
            (True, True) => true,
            (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Int(a), Float(b)) => a as f64 == b,
            (Float(a), Int(b)) => a == b as f64,
            _ => false,
        }
    }

    pub fn num_less(&self, other: &Self) -> Result<bool> {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(a < b),
            (Int(a), Float(b)) => Ok((a as f64) < b),
            (Float(a), Int(b)) => Ok(a < (b as f64)),
            (Float(a), Float(b)) => Ok(a < b),
            _ => Err(TypeError::WrongType.into()),
        }
    }

    pub fn set(&self, value: Scm) -> Result<()> {
        match self.value {
            ScmValue::Cell(x) => Ok(x.set(value)),
            _ => Err(TypeError::WrongType.into()),
        }
    }

    pub fn get(&self) -> Result<Scm> {
        match self.value {
            ScmValue::Cell(x) => Ok(x.get()),
            _ => Err(TypeError::WrongType.into()),
        }
    }

    pub fn invoke(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        match self.value {
            ScmValue::Closure(cls) => cls.invoke(nargs, vm),
            ScmValue::Primitive(func) => func.invoke(nargs, vm)?,
            ScmValue::Continuation(cnt) => cnt.invoke(nargs, vm)?,
            ScmValue::ExitProc(cnt) => cnt.invoke(nargs, vm)?,
            _ => return Err(TypeError::NotCallable(*self).into()),
        }
        Ok(())
    }

    pub fn invoke_tail(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        match self.value {
            ScmValue::Closure(cls) => cls.invoke_tail(nargs, vm),
            ScmValue::Primitive(func) => func.invoke_tail(nargs, vm)?,
            ScmValue::Continuation(cnt) => cnt.invoke_tail(nargs, vm)?,
            ScmValue::ExitProc(cnt) => cnt.invoke_tail(nargs, vm)?,
            _ => return Err(TypeError::NotCallable(*self).into()),
        }
        Ok(())
    }

    pub fn caar(&self) -> Result<Scm> {
        self.car()?.car()
    }

    pub fn cadr(&self) -> Result<Scm> {
        self.cdr()?.car()
    }

    pub fn cdar(&self) -> Result<Scm> {
        self.car()?.cdr()
    }

    pub fn cddr(&self) -> Result<Scm> {
        self.cdr()?.cdr()
    }

    pub fn display(&self) -> ScmWriteShared<ScmDisplay> {
        ScmWriteShared::new_cyclic(self.value)
    }

    pub fn write_simple(&self) -> ScmWriteSimple {
        ScmWriteSimple::new(self.value)
    }

    pub fn write_shared(&self) -> ScmWriteShared<ScmWriteSimple> {
        ScmWriteShared::new_shared(self.value)
    }

    pub fn write(&self) -> ScmWriteShared<ScmWriteSimple> {
        ScmWriteShared::new_cyclic(self.value)
    }
}

impl ScmValue {
    pub fn ptr_eq(&self, other: &Self) -> bool {
        use ScmValue::*;
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Symbol(a), Symbol(b)) => a.ptr_eq(b),
            (String(a), String(b)) => *a as *const str == *b as *const str,
            (Vector(a), Vector(b)) => *a as *const _ == *b as *const _,
            (Pair(a), Pair(b)) => *a as *const _ == *b as *const _,
            (Primitive(a), Primitive(b)) => a == b,
            (Cell(a), Cell(b)) => *a as *const _ == *b as *const _,
            (Error(a), Error(b)) => *a as *const _ == *b as *const _,
            _ => false,
        }
    }
}

impl std::fmt::Debug for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.display())
    }
}

impl From<&Scm> for Scm {
    fn from(scm: &Scm) -> Scm {
        *scm
    }
}

impl From<bool> for Scm {
    fn from(b: bool) -> Scm {
        Scm::bool(b)
    }
}

impl From<char> for Scm {
    fn from(ch: char) -> Scm {
        ScmValue::Char(ch).into()
    }
}

impl From<u8> for Scm {
    fn from(x: u8) -> Scm {
        Scm::int(x as i64)
    }
}

impl From<i32> for Scm {
    fn from(x: i32) -> Scm {
        Scm::int(x as i64)
    }
}

impl From<i64> for Scm {
    fn from(x: i64) -> Scm {
        Scm::int(x)
    }
}

impl From<usize> for Scm {
    fn from(x: usize) -> Scm {
        Scm::int(x as i64)
    }
}

impl From<Vec<u8>> for Scm {
    fn from(x: Vec<u8>) -> Scm {
        let x = Box::leak(x.into_boxed_slice());
        ScmValue::Bytevector(x).into()
    }
}

impl From<String> for Scm {
    fn from(s: String) -> Scm {
        let s = Box::leak(s.into_boxed_str());
        ScmValue::String(s).into()
    }
}

impl From<SchemePort> for Scm {
    fn from(p: SchemePort) -> Scm {
        let p = Box::leak(Box::new(p));
        ScmValue::Port(p).into()
    }
}

impl From<&Sexpr> for Scm {
    fn from(e: &Sexpr) -> Self {
        match e {
            Sexpr::Undefined => Scm::undefined(),
            Sexpr::Uninitialized => Scm::uninitialized(),
            Sexpr::Nil => Scm::nil(),
            Sexpr::True => Scm::bool(true),
            Sexpr::False => Scm::bool(false),
            Sexpr::Int(i) => Scm::int(*i),
            Sexpr::Float(f) => Scm::float(*f),
            Sexpr::Symbol(s) => Scm::symbol(*s),
            Sexpr::String(s) => Scm::string(&**s),
            Sexpr::Pair(p) => Scm::cons((&p.0).into(), (&p.1).into()),
            Sexpr::Vector(v) => {
                let items: Vec<Cell<Scm>> = v.iter().map(|i| Cell::new(i.into())).collect();
                let items = items.into_boxed_slice();
                ScmValue::Vector(Box::leak(items)).into()
            }
            Sexpr::SyntacticClosure(sc) => {
                unimplemented!("convert syntactic closure to Scm: <{}>", sc.sexpr())
            }
        }
    }
}

impl From<&TrackedSexpr> for Scm {
    fn from(e: &TrackedSexpr) -> Self {
        (&e.sexpr).into()
    }
}

impl From<&crate::parsing::Sexpr<'_>> for Scm {
    fn from(e: &crate::parsing::Sexpr<'_>) -> Self {
        use crate::parsing::Sexpr::*;
        match e {
            Nil => Scm::nil(),
            True => Scm::bool(true),
            False => Scm::bool(false),
            Integer(i) => Scm::int(*i),
            Float(f) => Scm::float(*f),
            Symbol(s) => Scm::symbol(*s),
            String(s) => Scm::string(s.to_owned()),
            List(l) => {
                let mut out_list = Scm::nil();
                for x in l.into_iter().rev() {
                    if let Dot = x.expr {
                        out_list = out_list.car().unwrap();
                    } else {
                        out_list = Scm::cons(x.into(), out_list);
                    }
                }
                out_list
            }
            Vector(v) => {
                let items: Vec<Cell<Scm>> = v.iter().map(|i| Cell::new(i.into())).collect();
                let items = items.into_boxed_slice();
                ScmValue::Vector(Box::leak(items)).into()
            }
            Dot => unimplemented!(),
        }
    }
}

impl From<&crate::parsing::SpannedSexpr<'_>> for Scm {
    fn from(e: &crate::parsing::SpannedSexpr<'_>) -> Self {
        (&e.expr).into()
    }
}

impl std::ops::Mul for Scm {
    type Output = Result<Scm>;
    fn mul(self, other: Self) -> Self::Output {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(Scm::int(a * b)),
            (Int(a), Float(b)) => Ok(Scm::float(a as f64 * b)),
            (Float(a), Int(b)) => Ok(Scm::float(a * b as f64)),
            (Float(a), Float(b)) => Ok(Scm::float(a * b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl std::ops::Div for Scm {
    type Output = Result<Scm>;
    fn div(self, other: Self) -> Self::Output {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(Scm::float(a as f64 / b as f64)),
            (Int(a), Float(b)) => Ok(Scm::float(a as f64 / b)),
            (Float(a), Int(b)) => Ok(Scm::float(a / b as f64)),
            (Float(a), Float(b)) => Ok(Scm::float(a / b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl std::ops::Rem for Scm {
    type Output = Result<Scm>;
    fn rem(self, other: Self) -> Self::Output {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(Scm::int(a % b)),
            (Int(a), Float(b)) => Ok(Scm::float(a as f64 % b)),
            (Float(a), Int(b)) => Ok(Scm::float(a % b as f64)),
            (Float(a), Float(b)) => Ok(Scm::float(a % b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl std::ops::Add for Scm {
    type Output = Result<Scm>;
    fn add(self, other: Self) -> Self::Output {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(Scm::int(a + b)),
            (Int(a), Float(b)) => Ok(Scm::float(a as f64 + b)),
            (Float(a), Int(b)) => Ok(Scm::float(a + b as f64)),
            (Float(a), Float(b)) => Ok(Scm::float(a + b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl std::ops::Sub for Scm {
    type Output = Result<Scm>;
    fn sub(self, other: Self) -> Self::Output {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(Scm::int(a - b)),
            (Int(a), Float(b)) => Ok(Scm::float(a as f64 - b)),
            (Float(a), Int(b)) => Ok(Scm::float(a - b as f64)),
            (Float(a), Float(b)) => Ok(Scm::float(a - b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

pub trait ResultWrap {
    fn wrap(self) -> Result<Scm>;
}

impl<T> ResultWrap for T
where
    T: Into<Scm>,
{
    fn wrap(self) -> Result<Scm> {
        Ok(self.into())
    }
}

impl<T> ResultWrap for Result<T>
where
    T: Into<Scm>,
{
    fn wrap(self) -> Result<Scm> {
        self.map(T::into)
    }
}

impl ResultWrap for () {
    fn wrap(self) -> Result<Scm> {
        Ok(Scm::undefined())
    }
}

impl ResultWrap for Result<()> {
    fn wrap(self) -> Result<Scm> {
        self.map(|_| Scm::undefined())
    }
}

impl PartialEq for Scm {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other)
    }
}

impl TryFrom<Scm> for usize {
    type Error = crate::error::Error;
    fn try_from(scm: Scm) -> Result<Self> {
        match scm.value {
            ScmValue::Int(i) if i >= 0 => Ok(i as usize),
            _ => Err(TypeError::NoPositiveInt(scm).into()),
        }
    }
}

impl TryFrom<&Scm> for usize {
    type Error = crate::error::Error;
    fn try_from(scm: &Scm) -> Result<Self> {
        Self::try_from(*scm)
    }
}
