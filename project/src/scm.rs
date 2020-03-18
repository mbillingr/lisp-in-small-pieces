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
use crate::syntactic_closure::SyntacticClosure;

pub type Scm = ScmContainer<()>;

#[derive(Copy, Clone)]
pub struct ScmContainer<T: 'static> {
    pub value: ScmValue<T>,
    pub meta_data: T,
}

#[derive(Copy, Clone)]
pub enum ScmValue<T: 'static> {
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
    Vector(&'static [Cell<ScmContainer<T>>]),
    Bytevector(&'static [u8]),

    Pair(&'static (Cell<ScmContainer<T>>, Cell<ScmContainer<T>>)),

    Closure(&'static Closure),
    Primitive(RuntimePrimitive),
    Continuation(&'static Continuation),
    ExitProc(&'static ExitProcedure),

    Port(&'static SchemePort),

    Cell(&'static Cell<ScmContainer<T>>),
    Rust(&'static Box<dyn Any>),
    Error(&'static Error),

    SyntacticClosure(&'static SyntacticClosure),
}

impl<T: Copy + Default> ScmValue<T> {
    fn from_scmval<M: Copy>(value: ScmValue<M>) -> Self {
        use ScmValue::*;
        match value {
            Undefined => Undefined,
            Uninitialized => Uninitialized,
            Nil => Nil,
            True => True,
            False => False,
            Eof => Eof,
            Int(i) => Int(i),
            Float(f) => Float(f),
            Char(ch) => Char(ch),
            Symbol(s) => Symbol(s),
            String(s) => String(s),
            Vector(items) => Self::vector(items.iter().map(|x| ScmContainer::from_scm(x.get()))),
            Bytevector(bv) => Bytevector(bv),
            Pair(p) => Self::cons(ScmContainer::from_scm(p.0.get()), ScmContainer::from_scm(p.1.get())),
            Closure(c) => Closure(c),
            Primitive(p) => Primitive(p),
            Continuation(c) => Continuation(c),
            ExitProc(ep) => ExitProc(ep),
            Port(p) => Port(p),
            Cell(c) => Self::boxed(ScmContainer::from_scm(c.get())),
            Rust(obj) => Rust(obj),
            Error(e) => Error(e),
            SyntacticClosure(sc) => SyntacticClosure(sc),
        }
    }
}

impl<M: Copy, T: Copy + Default> From<ScmValue<M>> for ScmContainer<T> {
    fn from(value: ScmValue<M>) -> Self {
        ScmContainer {
            value: ScmValue::<T>::from_scmval(value),
            meta_data: Default::default(),
        }
    }
}

impl<M: Copy + Default> ScmContainer<M> {
    pub fn from_scm<T: Copy>(scm: ScmContainer<T>) -> Self {
        scm.value.into()
    }

    pub fn uninitialized() -> Self {
        ScmValue::<M>::Uninitialized.into()
    }

    pub fn undefined() -> Self {
        ScmValue::<M>::Undefined.into()
    }

    pub fn nil() -> Self {
        ScmValue::<M>::Nil.into()
    }

    pub fn bool(b: bool) -> Self {
        match b {
            true => ScmValue::<M>::True,
            false => ScmValue::<M>::False,
        }
        .into()
    }

    pub fn int(x: i64) -> Self {
        ScmValue::<M>::Int(x).into()
    }

    pub fn float(x: f64) -> Self {
        ScmValue::<M>::Float(x).into()
    }

    pub fn eof() -> Self {
        ScmValue::<M>::Eof.into()
    }

    pub fn boxed(x: Self) -> Self {
        ScmValue::<M>::boxed(x).into()
    }

    pub fn cons(car: Self, cdr: Self) -> Self {
        ScmValue::<M>::cons(car, cdr).into()
    }

    pub fn closure(func: &'static CodeObject, free_vars: impl Into<Box<[Scm]>>) -> Self {
        ScmValue::<M>::Closure(Box::leak(Box::new(Closure::new(func, free_vars.into())))).into()
    }

    pub fn symbol(s: impl Into<Symbol>) -> Self {
        ScmValue::<M>::Symbol(s.into()).into()
    }

    pub fn string(s: impl Into<Box<str>>) -> Self {
        ScmValue::<M>::String(Box::leak(s.into())).into()
    }

    pub fn error(e: impl Into<Error>) -> Self {
        ScmValue::<M>::Error(Box::leak(Box::new(e.into()))).into()
    }

    pub fn list<T, I>(items: T) -> Self
    where
        T: IntoIterator<Item = Self, IntoIter = I>,
        I: DoubleEndedIterator<Item = Self>,
    {
        let mut out = Self::nil();
        for x in items.into_iter().rev() {
            out = Self::cons(x, out);
        }
        out
    }

    pub fn vector(items: impl IntoIterator<Item = Self>) -> Self {
        ScmValue::<_>::vector(items).into()
    }

    pub fn primitive(proc: RuntimePrimitive) -> Self {
        ScmValue::<M>::Primitive(proc).into()
    }

    pub fn continuation(c: &'static Continuation) -> Self {
        ScmValue::<M>::Continuation(c).into()
    }

    pub fn exit_proc(ep: &'static ExitProcedure) -> Self {
        ScmValue::<M>::ExitProc(ep).into()
    }

    pub fn rust_object<T: 'static>(obj: T) -> Self {
        let boxed: Box<dyn Any> = Box::new(obj);
        let obj = Box::leak(Box::new(boxed));
        ScmValue::<M>::Rust(obj).into()
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
            _ => Err(TypeError::NoChar(ScmContainer::from_scm(*self)).into()),
        }
    }

    pub fn is_symbol(&self) -> bool {
        self.as_symbol().is_ok()
    }

    pub fn as_symbol(&self) -> Result<Symbol> {
        match self.value {
            ScmValue::Symbol(s) => Ok(s),
            _ => Err(TypeError::NoSymbol.into()),
        }
    }

    pub fn cmp_symbol(&self, s: &str) -> bool {
        self.as_symbol().map(|sy| sy == s).unwrap_or(false)
    }

    pub fn as_string(&self) -> Result<&'static str> {
        match self.value {
            ScmValue::String(s) => Ok(s),
            _ => Err(TypeError::NoString(ScmContainer::from_scm(*self)).into()),
        }
    }

    pub fn is_rust_object(&self) -> bool {
        self.as_rust_object().is_ok()
    }

    pub fn as_rust_object(&self) -> Result<&'static dyn Any> {
        match self.value {
            ScmValue::Rust(o) => Ok(&**o),
            _ => Err(TypeError::NoRustObject(ScmContainer::from_scm(*self)).into()),
        }
    }

    pub fn is_pair(&self) -> bool {
        match self.value {
            ScmValue::Pair(_) => true,
            _ => false,
        }
    }

    pub fn car(&self) -> Result<Self> {
        match self.value {
            ScmValue::Pair(p) => Ok(p.0.get()),
            _ => Err(TypeError::NoPair(ScmContainer::from_scm(*self)).into()),
        }
    }

    pub fn cdr(&self) -> Result<Self> {
        match self.value {
            ScmValue::Pair(p) => Ok(p.1.get()),
            _ => Err(TypeError::NoPair(ScmContainer::from_scm(*self)).into()),
        }
    }

    pub fn set_car(&self, x: Self) -> Result<Self> {
        match self.value {
            ScmValue::Pair(p) => {
                p.0.set(x);
                Ok(Self::undefined())
            }
            _ => Err(TypeError::NoPair(ScmContainer::from_scm(*self)).into()),
        }
    }

    pub fn set_cdr(&self, x: Self) -> Result<Self> {
        match self.value {
            ScmValue::Pair(p) => {
                p.1.set(x);
                Ok(Self::undefined())
            }
            _ => Err(TypeError::NoPair(ScmContainer::from_scm(*self)).into()),
        }
    }

    pub fn is_vector(&self) -> bool {
        self.as_vector().is_ok()
    }

    pub fn as_vector(&self) -> Result<&'static [Cell<Self>]> {
        match self.value {
            ScmValue::Vector(v) => Ok(v),
            _ => Err(TypeError::NoVector.into()),
        }
    }

    pub fn vector_ref(&self, idx: usize) -> Result<Self> {
        match self.value {
            ScmValue::Vector(v) => v
                .get(idx)
                .map(Cell::get)
                .ok_or(TypeError::OutOfBounds.into()),
            _ => Err(TypeError::NoVector.into()),
        }
    }

    pub fn vector_set(&self, idx: usize, val: Self) -> Result<()> {
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
            _ => Err(TypeError::NoPort(ScmContainer::from_scm(*self)).into()),
        }
    }

    pub fn as_bytevec(&self) -> Result<&'static [u8]> {
        match self.value {
            ScmValue::Bytevector(v) => Ok(v),
            _ => Err(TypeError::NoBytevector(ScmContainer::from_scm(*self)).into()),
        }
    }

    pub fn as_mut_bytevec(&self) -> Result<&'static mut [u8]> {
        match self.value {
            ScmValue::Bytevector(v) => unsafe {
                let ptr = v as *const _;
                let mut_ptr = ptr as *mut _;
                Ok(&mut *mut_ptr)
            },
            _ => Err(TypeError::NoBytevector(ScmContainer::from_scm(*self)).into()),
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

    pub fn set(&self, value: Self) -> Result<()> {
        match self.value {
            ScmValue::Cell(x) => Ok(x.set(value)),
            _ => Err(TypeError::WrongType.into()),
        }
    }

    pub fn get(&self) -> Result<Self> {
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
            _ => return Err(TypeError::NotCallable(ScmContainer::from_scm(*self)).into()),
        }
        Ok(())
    }

    pub fn invoke_tail(&self, nargs: usize, vm: &mut VirtualMachine) -> Result<()> {
        match self.value {
            ScmValue::Closure(cls) => cls.invoke_tail(nargs, vm),
            ScmValue::Primitive(func) => func.invoke_tail(nargs, vm)?,
            ScmValue::Continuation(cnt) => cnt.invoke_tail(nargs, vm)?,
            ScmValue::ExitProc(cnt) => cnt.invoke_tail(nargs, vm)?,
            _ => return Err(TypeError::NotCallable(ScmContainer::from_scm(*self)).into()),
        }
        Ok(())
    }

    pub fn caar(&self) -> Result<Self> {
        self.car()?.car()
    }

    pub fn cadr(&self) -> Result<Self> {
        self.cdr()?.car()
    }

    pub fn cdar(&self) -> Result<Self> {
        self.car()?.cdr()
    }

    pub fn cddr(&self) -> Result<Self> {
        self.cdr()?.cdr()
    }

    pub fn display(&self) -> ScmWriteShared<ScmDisplay> {
        ScmWriteShared::new_cyclic(self.value.into())
    }

    pub fn write_simple(&self) -> ScmWriteSimple {
        ScmWriteSimple::new(self.value.into())
    }

    pub fn write_shared(&self) -> ScmWriteShared<ScmWriteSimple> {
        ScmWriteShared::new_shared(self.value.into())
    }

    pub fn write(&self) -> ScmWriteShared<ScmWriteSimple> {
        ScmWriteShared::new_cyclic(self.value.into())
    }
}

impl<T> ScmValue<T> {
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

    pub fn cons(car: impl Into<ScmContainer<T>>, cdr: impl Into<ScmContainer<T>>) -> Self {
        ScmValue::Pair(Box::leak(Box::new((Cell::new(car.into()), Cell::new(cdr.into())))))
    }

    pub fn vector(items: impl IntoIterator<Item = ScmContainer<T>>) -> Self {
        let v: Vec<Cell<_>> = items.into_iter().map(Cell::new).collect();
        let static_data = Box::leak(v.into_boxed_slice());
        ScmValue::Vector(static_data)
    }

    pub fn boxed(x: impl Into<ScmContainer<T>>) -> Self {
        ScmValue::Cell(Box::leak(Box::new(Cell::new(x.into()))))
    }
}

impl<T: Default + Copy> std::fmt::Debug for ScmContainer<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.display())
    }
}

impl<T> From<&ScmContainer<T>> for ScmContainer<T> {
    fn from(scm: &ScmContainer<T>) -> Self {
        *scm
    }
}

impl<T: Default + Copy> From<bool> for ScmContainer<T> {
    fn from(b: bool) -> Self {
        ScmContainer::bool(b)
    }
}

impl<T: Default + Copy> From<char> for ScmContainer<T> {
    fn from(ch: char) -> Self {
        ScmValue::Char(ch).into()
    }
}

impl<T: Default + Copy> From<u8> for ScmContainer<T> {
    fn from(x: u8) -> Self {
        ScmContainer::int(x as i64)
    }
}

impl<T: Default + Copy> From<i32> for ScmContainer<T> {
    fn from(x: i32) -> Self {
        ScmContainer::int(x as i64)
    }
}

impl<T: Default + Copy> From<i64> for ScmContainer<T> {
    fn from(x: i64) -> Self {
        ScmContainer::int(x)
    }
}

impl<T: Default + Copy> From<usize> for ScmContainer<T> {
    fn from(x: usize) -> Self {
        ScmContainer::int(x as i64)
    }
}

impl<T: Default + Copy> From<Vec<u8>> for ScmContainer<T> {
    fn from(x: Vec<u8>) -> Self {
        let x = Box::leak(x.into_boxed_slice());
        ScmValue::Bytevector(x).into()
    }
}

impl<T: Default + Copy> From<String> for ScmContainer<T> {
    fn from(s: String) -> Self {
        let s = Box::leak(s.into_boxed_str());
        ScmValue::String(s).into()
    }
}

impl<T: Default + Copy> From<SchemePort> for ScmContainer<T> {
    fn from(p: SchemePort) -> Self {
        let p = Box::leak(Box::new(p));
        ScmValue::Port(p).into()
    }
}

impl<T> From<&Sexpr> for ScmContainer<T> {
    fn from(e: &Sexpr) -> Self {
        match e {
            Sexpr::Undefined => ScmContainer::undefined(),
            Sexpr::Uninitialized => ScmContainer::uninitialized(),
            Sexpr::Nil => ScmContainer::nil(),
            Sexpr::True => ScmContainer::bool(true),
            Sexpr::False => ScmContainer::bool(false),
            Sexpr::Int(i) => ScmContainer::int(*i),
            Sexpr::Float(f) => ScmContainer::float(*f),
            Sexpr::Symbol(s) => ScmContainer::symbol(*s),
            Sexpr::String(s) => ScmContainer::string(&**s),
            Sexpr::Pair(p) => ScmContainer::cons((&p.0).into(), (&p.1).into()),
            Sexpr::Vector(v) => {
                let items: Vec<Cell<ScmContainer>> = v.iter().map(|i| Cell::new(i.into())).collect();
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

impl<T> From<&crate::parsing::Sexpr<'_>> for ScmContainer<T> {
    fn from(e: &crate::parsing::Sexpr<'_>) -> Self {
        use crate::parsing::Sexpr::*;
        match e {
            Nil => ScmContainer::nil(),
            True => ScmContainer::bool(true),
            False => ScmContainer::bool(false),
            Integer(i) => ScmContainer::int(*i),
            Float(f) => ScmContainer::float(*f),
            Symbol(s) => ScmContainer::symbol(*s),
            String(s) => ScmContainer::string(s.to_owned()),
            List(l) => {
                let mut out_list = ScmContainer::nil();
                for x in l.into_iter().rev() {
                    if let Dot = x.expr {
                        out_list = out_list.car().unwrap();
                    } else {
                        out_list = ScmContainer::cons(x.into(), out_list);
                    }
                }
                out_list
            }
            Vector(v) => {
                let items: Vec<Cell<ScmContainer>> = v.iter().map(|i| Cell::new(i.into())).collect();
                let items = items.into_boxed_slice();
                ScmValue::Vector(Box::leak(items)).into()
            }
            Dot => unimplemented!(),
        }
    }
}

impl<T> From<&crate::parsing::SpannedSexpr<'_>> for ScmContainer<T> {
    fn from(e: &crate::parsing::SpannedSexpr<'_>) -> Self {
        (&e.expr).into()
    }
}

impl<T> std::ops::Mul for ScmContainer<T> {
    type Output = Result<Self>;
    fn mul(self, other: Self) -> Self::Output {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(ScmContainer::int(a * b)),
            (Int(a), Float(b)) => Ok(ScmContainer::float(a as f64 * b)),
            (Float(a), Int(b)) => Ok(ScmContainer::float(a * b as f64)),
            (Float(a), Float(b)) => Ok(ScmContainer::float(a * b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl<T> std::ops::Div for ScmContainer<T> {
    type Output = Result<Self>;
    fn div(self, other: Self) -> Self::Output {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(ScmContainer::float(a as f64 / b as f64)),
            (Int(a), Float(b)) => Ok(ScmContainer::float(a as f64 / b)),
            (Float(a), Int(b)) => Ok(ScmContainer::float(a / b as f64)),
            (Float(a), Float(b)) => Ok(ScmContainer::float(a / b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl<T> std::ops::Rem for ScmContainer<T> {
    type Output = Result<Self>;
    fn rem(self, other: Self) -> Self::Output {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(ScmContainer::int(a % b)),
            (Int(a), Float(b)) => Ok(ScmContainer::float(a as f64 % b)),
            (Float(a), Int(b)) => Ok(ScmContainer::float(a % b as f64)),
            (Float(a), Float(b)) => Ok(ScmContainer::float(a % b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl<T> std::ops::Add for ScmContainer<T> {
    type Output = Result<Self>;
    fn add(self, other: Self) -> Self::Output {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(ScmContainer::int(a + b)),
            (Int(a), Float(b)) => Ok(ScmContainer::float(a as f64 + b)),
            (Float(a), Int(b)) => Ok(ScmContainer::float(a + b as f64)),
            (Float(a), Float(b)) => Ok(ScmContainer::float(a + b)),
            _ => Err(TypeError::WrongType.into()),
        }
    }
}

impl<T> std::ops::Sub for ScmContainer<T> {
    type Output = Result<Self>;
    fn sub(self, other: Self) -> Self::Output {
        use ScmValue::*;
        match (self.value, other.value) {
            (Int(a), Int(b)) => Ok(ScmContainer::int(a - b)),
            (Int(a), Float(b)) => Ok(ScmContainer::float(a as f64 - b)),
            (Float(a), Int(b)) => Ok(ScmContainer::float(a - b as f64)),
            (Float(a), Float(b)) => Ok(ScmContainer::float(a - b)),
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
        Ok(ScmContainer::undefined())
    }
}

impl ResultWrap for Result<()> {
    fn wrap(self) -> Result<Scm> {
        self.map(|_| ScmContainer::undefined())
    }
}

impl<T> PartialEq for ScmContainer<T> {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other)
    }
}

impl<T> TryFrom<ScmContainer<T>> for usize {
    type Error = crate::error::Error;
    fn try_from(scm: ScmContainer<T>) -> Result<Self> {
        match scm.value {
            ScmValue::Int(i) if i >= 0 => Ok(i as usize),
            _ => Err(TypeError::NoPositiveInt(scm).into()),
        }
    }
}

impl<T> TryFrom<&ScmContainer<T>> for usize {
    type Error = crate::error::Error;
    fn try_from(scm: &ScmContainer<T>) -> Result<Self> {
        Self::try_from(*scm)
    }
}
