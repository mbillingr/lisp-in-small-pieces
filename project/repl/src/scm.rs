use crate::bytecode::{Closure, CodeObject, VirtualMachine};
use crate::continuation::{Continuation, ExitProcedure};
use crate::error::{Error, Result, TypeError};
use crate::ports::SchemePort;
use crate::primitive::RuntimePrimitive;
use crate::scm_write::{ScmDisplay, ScmWriteShared, ScmWriteSimple};
use crate::sexpr::{Sexpr, TrackedSexpr};
use std::any::Any;
use std::cell::Cell;
use std::convert::TryFrom;
use sunny_common::Symbol;

#[derive(Copy, Clone)]
pub enum Scm {
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
    String(&'static Cell<&'static str>),
    Vector(&'static [Cell<Scm>]),
    Bytevector(&'static [u8]),

    Pair(&'static (Cell<Scm>, Cell<Scm>)),

    Closure(&'static Closure),
    Primitive(RuntimePrimitive),
    Continuation(&'static Continuation),
    ExitProc(&'static ExitProcedure),

    Port(&'static SchemePort),

    Cell(&'static Cell<Scm>),
    Rust(&'static Box<dyn Any>),
    Error(&'static Error),
}

impl Scm {
    pub fn uninitialized() -> Self {
        Scm::Uninitialized
    }

    pub fn nil() -> Self {
        Self::Nil
    }

    pub fn bool(b: bool) -> Self {
        match b {
            true => Scm::True,
            false => Scm::False,
        }
    }

    pub fn boxed(x: Scm) -> Scm {
        Scm::Cell(Box::leak(Box::new(Cell::new(x))))
    }

    pub fn cons(car: Scm, cdr: Scm) -> Scm {
        Scm::Pair(Box::leak(Box::new((Cell::new(car), Cell::new(cdr)))))
    }

    pub fn closure(func: &'static CodeObject, free_vars: impl Into<Box<[Scm]>>) -> Self {
        Scm::Closure(Box::leak(Box::new(Closure::new(func, free_vars.into()))))
    }

    pub fn symbol(s: impl Into<Symbol>) -> Self {
        Scm::Symbol(s.into())
    }

    pub fn string(s: impl Into<Box<str>>) -> Self {
        let static_str: &str = Box::leak(s.into());
        Scm::str(static_str)
    }

    pub fn str(s: &'static str) -> Self {
        Scm::String(Box::leak(Box::new(Cell::new(s))))
    }

    pub fn error(e: impl Into<Error>) -> Self {
        Scm::Error(Box::leak(Box::new(e.into())))
    }

    pub fn list<T, I>(items: T) -> Self
    where
        T: IntoIterator<Item = Scm, IntoIter = I>,
        I: DoubleEndedIterator<Item = Scm>,
    {
        let mut out = Scm::Nil;
        for x in items.into_iter().rev() {
            out = Scm::cons(x, out);
        }
        out
    }

    pub fn vector(items: impl IntoIterator<Item = Scm>) -> Self {
        let v: Vec<Cell<Scm>> = items.into_iter().map(Cell::new).collect();
        let static_data = Box::leak(v.into_boxed_slice());
        Scm::Vector(static_data)
    }

    pub fn primitive(proc: RuntimePrimitive) -> Self {
        Scm::Primitive(proc)
    }

    pub fn rust_object<T: 'static>(obj: T) -> Self {
        let boxed: Box<dyn Any> = Box::new(obj);
        let obj = Box::leak(Box::new(boxed));
        Scm::Rust(obj)
    }

    pub fn is_undefined(&self) -> bool {
        match self {
            Scm::Undefined => true,
            _ => false,
        }
    }

    pub fn is_uninitialized(&self) -> bool {
        match self {
            Scm::Uninitialized => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Scm::Nil => true,
            _ => false,
        }
    }

    pub fn is_eof(&self) -> bool {
        match self {
            Scm::Eof => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Scm::True | Scm::False => true,
            _ => false,
        }
    }

    pub fn is_false(&self) -> bool {
        match self {
            Scm::False => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Scm::Int(_) => true,
            Scm::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_procedure(&self) -> bool {
        match self {
            Scm::Closure(_) | Scm::Primitive(_) | Scm::Continuation(_) | Scm::ExitProc(_) => true,
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Scm::Primitive(_) => true,
            _ => false,
        }
    }

    pub fn is_cell(&self) -> bool {
        match self {
            Scm::Cell(_) => true,
            _ => false,
        }
    }

    pub fn as_int(&self) -> Result<i64> {
        match self {
            Scm::Int(i) => Ok(*i),
            _ => Err(TypeError::NoInt.into()),
        }
    }

    pub fn as_char(&self) -> Result<char> {
        match self {
            Scm::Char(ch) => Ok(*ch),
            _ => Err(TypeError::NoChar(*self).into()),
        }
    }

    pub fn as_symbol(&self) -> Result<Symbol> {
        match self {
            Scm::Symbol(s) => Ok(*s),
            _ => Err(TypeError::NoSymbol.into()),
        }
    }

    pub fn as_string(&self) -> Result<&'static str> {
        match self {
            Scm::String(s) => Ok(s.get()),
            _ => Err(TypeError::NoString(*self).into()),
        }
    }

    pub fn replace_string(&self, s: impl Into<Box<str>>) -> Result<()> {
        let static_str: &str = Box::leak(s.into());
        self.replace_str(static_str)
    }

    pub fn replace_str(&self, s: &'static str) -> Result<()> {
        match self {
            Scm::String(sc) => Ok(sc.set(s)),
            _ => Err(TypeError::NoString(*self).into()),
        }
    }

    pub fn is_rust_object(&self) -> bool {
        self.as_rust_object().is_ok()
    }

    pub fn as_rust_object(&self) -> Result<&'static dyn Any> {
        match self {
            Scm::Rust(o) => Ok(&***o),
            _ => Err(TypeError::NoRustObject(*self).into()),
        }
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Scm::Pair(_) => true,
            _ => false,
        }
    }

    pub fn car(&self) -> Result<Scm> {
        match self {
            Scm::Pair(p) => Ok(p.0.get()),
            _ => Err(TypeError::NoPair(*self).into()),
        }
    }

    pub fn cdr(&self) -> Result<Scm> {
        match self {
            Scm::Pair(p) => Ok(p.1.get()),
            _ => Err(TypeError::NoPair(*self).into()),
        }
    }

    pub fn set_car(&self, x: Scm) -> Result<Scm> {
        match self {
            Scm::Pair(p) => {
                p.0.set(x);
                Ok(Scm::Undefined)
            }
            _ => Err(TypeError::NoPair(*self).into()),
        }
    }

    pub fn set_cdr(&self, x: Scm) -> Result<Scm> {
        match self {
            Scm::Pair(p) => {
                p.1.set(x);
                Ok(Scm::Undefined)
            }
            _ => Err(TypeError::NoPair(*self).into()),
        }
    }

    pub fn is_vector(&self) -> bool {
        self.as_vector().is_ok()
    }

    pub fn as_vector(&self) -> Result<&'static [Cell<Scm>]> {
        match self {
            Scm::Vector(v) => Ok(*v),
            _ => Err(TypeError::NoVector.into()),
        }
    }

    pub fn vector_ref(&self, idx: usize) -> Result<Scm> {
        match self {
            Scm::Vector(v) => v
                .get(idx)
                .map(Cell::get)
                .ok_or(TypeError::OutOfBounds.into()),
            _ => Err(TypeError::NoVector.into()),
        }
    }

    pub fn vector_set(&self, idx: usize, val: Scm) -> Result<()> {
        match self {
            Scm::Vector(v) => match v.get(idx) {
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
        match self {
            Scm::Closure(cls) => Ok(*cls),
            _ => Err(TypeError::NoClosure.into()),
        }
    }

    pub fn as_port(&self) -> Result<&'static SchemePort> {
        match self {
            Scm::Port(p) => Ok(*p),
            _ => Err(TypeError::NoPort(*self).into()),
        }
    }

    pub fn as_bytevec(&self) -> Result<&'static [u8]> {
        match self {
            Scm::Bytevector(v) => Ok(*v),
            _ => Err(TypeError::NoBytevector(*self).into()),
        }
    }

    pub fn as_mut_bytevec(&self) -> Result<&'static mut [u8]> {
        match self {
            Scm::Bytevector(v) => unsafe {
                let ptr = *v as *const _;
                let mut_ptr = ptr as *mut _;
                Ok(&mut *mut_ptr)
            },
            _ => Err(TypeError::NoBytevector(*self).into()),
        }
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        use Scm::*;
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Symbol(a), Symbol(b)) => a.ptr_eq(b),
            (String(a), String(b)) => *a as *const _ == *b as *const _,
            (Vector(a), Vector(b)) => *a as *const _ == *b as *const _,
            (Pair(a), Pair(b)) => *a as *const _ == *b as *const _,
            (Primitive(a), Primitive(b)) => a == b,
            (Cell(a), Cell(b)) => *a as *const _ == *b as *const _,
            (Error(a), Error(b)) => *a as *const _ == *b as *const _,
            _ => false,
        }
    }

    pub fn equals(&self, other: &Self) -> bool {
        use Scm::*;
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Vector(a), Vector(b)) => a.iter().zip(*b).all(|(a, b)| a.get().equals(&b.get())),
            (Pair(a), Pair(b)) => a.0.get().equals(&b.0.get()) && a.1.get().equals(&b.1.get()),
            (Primitive(a), Primitive(b)) => a == b,
            (Cell(a), Cell(b)) => a.get().equals(&b.get()),
            (Error(a), Error(b)) => a == b,
            _ => false,
        }
    }

    pub fn num_eq(&self, other: &Self) -> bool {
        use Scm::*;
        match (self, other) {
            (True, True) => true,
            (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Int(a), Float(b)) => *a as f64 == *b,
            (Float(a), Int(b)) => *a == *b as f64,
            _ => false,
        }
    }

    pub fn num_less(&self, other: &Self) -> Result<bool> {
        use Scm::*;
        match (*self, *other) {
            (Int(a), Int(b)) => Ok(a < b),
            (Int(a), Float(b)) => Ok((a as f64) < b),
            (Float(a), Int(b)) => Ok(a < (b as f64)),
            (Float(a), Float(b)) => Ok(a < b),
            _ => Err(TypeError::WrongType.into()),
        }
    }

    pub fn set(&self, value: Scm) -> Result<()> {
        match self {
            Scm::Cell(x) => Ok(x.set(value)),
            _ => Err(TypeError::WrongType.into()),
        }
    }

    pub fn get(&self) -> Result<Scm> {
        match self {
            Scm::Cell(x) => Ok(x.get()),
            _ => Err(TypeError::WrongType.into()),
        }
    }

    pub fn invoke(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        match self {
            Scm::Closure(cls) => cls.invoke(nargs, vm),
            Scm::Primitive(func) => func.invoke(nargs, vm)?,
            Scm::Continuation(cnt) => cnt.invoke(nargs, vm)?,
            Scm::ExitProc(cnt) => cnt.invoke(nargs, vm)?,
            _ => return Err(TypeError::NotCallable(*self).into()),
        }
        Ok(())
    }

    pub fn invoke_tail(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        match self {
            Scm::Closure(cls) => cls.invoke_tail(nargs, vm),
            Scm::Primitive(func) => func.invoke_tail(nargs, vm)?,
            Scm::Continuation(cnt) => cnt.invoke_tail(nargs, vm)?,
            Scm::ExitProc(cnt) => cnt.invoke_tail(nargs, vm)?,
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
        ScmWriteShared::new_cyclic(*self)
    }

    pub fn write_simple(&self) -> ScmWriteSimple {
        ScmWriteSimple::new(*self)
    }

    pub fn write_shared(&self) -> ScmWriteShared<ScmWriteSimple> {
        ScmWriteShared::new_shared(*self)
    }

    pub fn write(&self) -> ScmWriteShared<ScmWriteSimple> {
        ScmWriteShared::new_cyclic(*self)
    }

    pub fn exact(&self) -> Result<Scm> {
        match self {
            Scm::Int(_) => Ok(*self),
            Scm::Float(x) => Ok(Scm::Int(*x as i64)),
            _ => Err(TypeError::NoNumber(*self).into()),
        }
    }

    pub fn round(&self) -> Result<Scm> {
        match self {
            Scm::Int(_) => Ok(*self),
            Scm::Float(x) => Ok(Scm::Float(x.round())),
            _ => Err(TypeError::NoNumber(*self).into()),
        }
    }

    pub fn sqrt(&self) -> Result<Scm> {
        match self {
            Scm::Int(x) => Ok(Scm::Float(f64::sqrt(*x as _))),
            Scm::Float(x) => Ok(Scm::Float(x.sqrt())),
            _ => Err(TypeError::NoNumber(*self).into()),
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
        Scm::Char(ch)
    }
}

impl From<u8> for Scm {
    fn from(x: u8) -> Scm {
        Scm::Int(x as i64)
    }
}

impl From<i32> for Scm {
    fn from(x: i32) -> Scm {
        Scm::Int(x as i64)
    }
}

impl From<i64> for Scm {
    fn from(x: i64) -> Scm {
        Scm::Int(x)
    }
}

impl From<usize> for Scm {
    fn from(x: usize) -> Scm {
        Scm::Int(x as i64)
    }
}

impl From<Vec<u8>> for Scm {
    fn from(x: Vec<u8>) -> Scm {
        let x = Box::leak(x.into_boxed_slice());
        Scm::Bytevector(x)
    }
}

impl From<String> for Scm {
    fn from(s: String) -> Scm {
        Scm::string(s)
    }
}

impl From<SchemePort> for Scm {
    fn from(p: SchemePort) -> Scm {
        let p = Box::leak(Box::new(p));
        Scm::Port(p)
    }
}

impl From<&Sexpr> for Scm {
    fn from(e: &Sexpr) -> Self {
        match e {
            Sexpr::Undefined => Scm::Undefined,
            Sexpr::Uninitialized => Scm::Uninitialized,
            Sexpr::Nil => Scm::Nil,
            Sexpr::True => Scm::True,
            Sexpr::False => Scm::False,
            Sexpr::Char(ch) => Scm::Char(*ch),
            Sexpr::Int(i) => Scm::Int(*i),
            Sexpr::Float(f) => Scm::Float(*f),
            Sexpr::Symbol(s) => Scm::Symbol(*s),
            Sexpr::String(s) => Scm::string(&**s),
            Sexpr::Pair(p) => Scm::cons((&p.0).into(), (&p.1).into()),
            Sexpr::Vector(v) => {
                let items: Vec<Cell<Scm>> = v.iter().map(|i| Cell::new(i.into())).collect();
                let items = items.into_boxed_slice();
                Scm::Vector(Box::leak(items))
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

impl From<&sunny_parser::Sexpr<'_>> for Scm {
    fn from(e: &sunny_parser::Sexpr<'_>) -> Self {
        use sunny_parser::Sexpr::*;
        match e {
            Nil => Scm::Nil,
            True => Scm::True,
            False => Scm::False,
            Integer(i) => Scm::Int(*i),
            Float(f) => Scm::Float(*f),
            Char(ch) => Scm::Char(*ch),
            Symbol(s) => Scm::Symbol(sunny_common::Symbol::from_str(s)),
            String(s) => Scm::string(s.to_owned()),
            List(l) => {
                let mut out_list = Scm::Nil;
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
                Scm::Vector(Box::leak(items))
            }
            Dot => unimplemented!(),
        }
    }
}

impl From<&sunny_parser::SpannedSexpr<'_>> for Scm {
    fn from(e: &sunny_parser::SpannedSexpr<'_>) -> Self {
        (&e.expr).into()
    }
}

impl std::ops::Mul for Scm {
    type Output = Result<Scm>;
    fn mul(self, other: Self) -> Self::Output {
        use Scm::*;
        match (self, other) {
            (Int(a), Int(b)) => Ok(Int(a * b)),
            (Int(a), Float(b)) => Ok(Float(a as f64 * b)),
            (Float(a), Int(b)) => Ok(Float(a * b as f64)),
            (Float(a), Float(b)) => Ok(Float(a * b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl std::ops::Div for Scm {
    type Output = Result<Scm>;
    fn div(self, other: Self) -> Self::Output {
        use Scm::*;
        match (self, other) {
            (Int(a), Int(b)) => Ok(Float(a as f64 / b as f64)),
            (Int(a), Float(b)) => Ok(Float(a as f64 / b)),
            (Float(a), Int(b)) => Ok(Float(a / b as f64)),
            (Float(a), Float(b)) => Ok(Float(a / b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl std::ops::Rem for Scm {
    type Output = Result<Scm>;
    fn rem(self, other: Self) -> Self::Output {
        use Scm::*;
        match (self, other) {
            (Int(a), Int(b)) => Ok(Float(a as f64 % b as f64)),
            (Int(a), Float(b)) => Ok(Float(a as f64 % b)),
            (Float(a), Int(b)) => Ok(Float(a % b as f64)),
            (Float(a), Float(b)) => Ok(Float(a % b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl std::ops::Add for Scm {
    type Output = Result<Scm>;
    fn add(self, other: Self) -> Self::Output {
        use Scm::*;
        match (self, other) {
            (Int(a), Int(b)) => Ok(Int(a + b)),
            (Int(a), Float(b)) => Ok(Float(a as f64 + b)),
            (Float(a), Int(b)) => Ok(Float(a + b as f64)),
            (Float(a), Float(b)) => Ok(Float(a + b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl std::ops::Sub for Scm {
    type Output = Result<Scm>;
    fn sub(self, other: Self) -> Self::Output {
        use Scm::*;
        match (self, other) {
            (Int(a), Int(b)) => Ok(Int(a - b)),
            (Int(a), Float(b)) => Ok(Float(a as f64 - b)),
            (Float(a), Int(b)) => Ok(Float(a - b as f64)),
            (Float(a), Float(b)) => Ok(Float(a - b)),
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
        Ok(Scm::Undefined)
    }
}

impl ResultWrap for Result<()> {
    fn wrap(self) -> Result<Scm> {
        self.map(|_| Scm::Undefined)
    }
}

impl PartialEq for Scm {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other)
    }
}

impl TryFrom<Scm> for char {
    type Error = crate::error::Error;
    fn try_from(scm: Scm) -> Result<Self> {
        match scm {
            Scm::Char(ch) => Ok(ch),
            _ => Err(TypeError::NoChar(scm).into()),
        }
    }
}

impl TryFrom<&Scm> for char {
    type Error = crate::error::Error;
    fn try_from(scm: &Scm) -> Result<Self> {
        Self::try_from(*scm)
    }
}

impl TryFrom<Scm> for usize {
    type Error = crate::error::Error;
    fn try_from(scm: Scm) -> Result<Self> {
        match scm {
            Scm::Int(i) if i >= 0 => Ok(i as usize),
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

impl TryFrom<Scm> for &str {
    type Error = crate::error::Error;
    fn try_from(scm: Scm) -> Result<Self> {
        match scm {
            Scm::String(s) => Ok(s.get()),
            _ => Err(TypeError::NoString(scm).into()),
        }
    }
}

impl TryFrom<&Scm> for &str {
    type Error = crate::error::Error;
    fn try_from(scm: &Scm) -> Result<Self> {
        Self::try_from(*scm)
    }
}
