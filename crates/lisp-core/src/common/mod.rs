#[macro_use]
mod conversions;
#[macro_use]
mod combined_lisp_ops;
pub use combined_lisp_ops::CombinedLispOps;
use std::ops::{Add, Div, Mul, Sub};

pub type Result<T> = std::result::Result<T, LispError>;

#[derive(Debug)]
pub enum LispError {
    ExpectedPair,
    IncomatibleTypes,
}

/// Interface trait for constructing and accessing Lisp Values
pub trait BasicLispValue: Sized {
    type Symbol;
    type Procedure;

    fn nil() -> Self;
    fn bool(b: bool) -> Self;
    fn char(ch: char) -> Self;
    fn symbol(s: Self::Symbol) -> Self;
    fn cons<A: Into<Self>, D: Into<Self>>(car: A, cdr: D) -> Self;

    fn as_bool(&self) -> Option<bool>;
    fn as_symbol(&self) -> Option<&Self::Symbol>;
    fn as_pair_mut(&self) -> Option<(&mut Self, &mut Self)>;
    fn as_procedure(&self) -> Option<&Self::Procedure>;

    fn is_null(&self) -> bool;
    fn is_number(&self) -> bool;
    fn is_bool(&self) -> bool {
        self.as_bool().is_some()
    }
    fn is_true(&self) -> bool {
        self.as_bool() == Some(true)
    }
    fn is_false(&self) -> bool {
        self.as_bool() == Some(false)
    }
    fn is_symbol(&self) -> bool {
        self.as_symbol().is_some()
    }
    fn is_pair(&self) -> bool {
        self.as_pair().is_some()
    }
    fn is_procedure(&self) -> bool {
        self.as_procedure().is_some()
    }

    fn as_pair(&self) -> Option<(&Self, &Self)> {
        self.as_pair_mut().map(|(car, cdr)| (car as &_, cdr as &_))
    }
    fn get_car_mut(&self) -> Result<&mut Self> {
        self.as_pair_mut()
            .map(|(car, _)| car)
            .ok_or(LispError::ExpectedPair)
    }
    fn get_cdr_mut(&self) -> Result<&mut Self> {
        self.as_pair_mut()
            .map(|(_, cdr)| cdr)
            .ok_or(LispError::ExpectedPair)
    }
    fn car(&self) -> Result<&Self> {
        self.as_pair()
            .map(|(car, _)| car)
            .ok_or(LispError::ExpectedPair)
    }
    fn cdr(&self) -> Result<&Self> {
        self.as_pair()
            .map(|(_, cdr)| cdr)
            .ok_or(LispError::ExpectedPair)
    }
    fn set_car(&self, item: Self) -> Result<()> {
        *self.get_car_mut()? = item;
        Ok(())
    }
    fn set_cdr(&self, item: Self) -> Result<()> {
        *self.get_cdr_mut()? = item;
        Ok(())
    }
}

macro_rules! binop {
    ($name:ident) => { binop!{$name, $name} };

    ($name:ident, $op:ident) => { binop!{$name, $op, Self} };

    ($name:ident, $op:ident, $out:ty) => {
        fn $name(&self, rhs: &Self) -> Result<$out> {
            if let Some(a) = self.as_int() {
                if let Some(b) = rhs.as_int() {
                    return Ok(Self::int(a.$op(b)))
                }
            }
            if let Some(a) = self.as_float() {
                if let Some(b) = rhs.as_float() {
                    return Ok(Self::float(a.$op(b)))
                }
            }
            Err(LispError::IncomatibleTypes)
        }
    }
}

pub trait NumericLispValue: BasicLispValue {
    type Exact: Add<Output = Self::Exact>
        + Sub<Output = Self::Exact>
        + Mul<Output = Self::Exact>
        + Div<Output = Self::Exact>
        + PartialOrd;
    type Inexact: Add<Output = Self::Inexact>
        + Sub<Output = Self::Inexact>
        + Mul<Output = Self::Inexact>
        + Div<Output = Self::Inexact>
        + PartialOrd;

    fn int(i: Self::Exact) -> Self;
    fn float(f: Self::Inexact) -> Self;

    fn as_int(&self) -> Option<Self::Exact>;
    fn as_float(&self) -> Option<Self::Inexact>;

    fn is_int(&self) -> bool {
        self.as_int().is_some()
    }
    fn is_float(&self) -> bool {
        self.as_float().is_some()
    }

    binop! {add}
    binop! {sub}
    binop! {mul}
    binop! {div}

    fn is_less(&self, rhs: &Self) -> Result<Self> {
        if let Some(a) = self.as_int() {
            if let Some(b) = rhs.as_int() {
                return Ok(Self::bool(a < b));
            }
        }
        if let Some(a) = self.as_float() {
            if let Some(b) = rhs.as_float() {
                return Ok(Self::bool(a < b));
            }
        }
        Err(LispError::IncomatibleTypes)
    }
}
