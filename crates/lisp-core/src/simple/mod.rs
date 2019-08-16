use crate::common::{BasicLispValue, NumericLispValue};
use lexpr;
use std::cell::UnsafeCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value<F> {
    Undefined,
    Nil,
    True,
    False,
    Int(i64),
    Float(f64),
    Symbol(&'static str),
    Pair(Rc<UnsafeCell<(Value<F>, Value<F>)>>),

    Function(F),
}

impl<F> BasicLispValue for Value<F> {
    type Symbol = &'static str;
    type Procedure = F;

    fn nil() -> Self {
        Value::Nil
    }

    fn bool(b: bool) -> Self {
        match b {
            true => Value::True,
            false => Value::False,
        }
    }

    fn char(_ch: char) -> Self {
        unimplemented!()
    }
    fn symbol(s: <Self as BasicLispValue>::Symbol) -> Self {
        Value::Symbol(s)
    }
    fn cons<A: Into<Self>, D: Into<Self>>(car: A, cdr: D) -> Self {
        Value::Pair(Rc::new(UnsafeCell::new((car.into(), cdr.into()))))
    }

    fn as_bool(&self) -> Option<bool> {
        match self {
            Value::True => Some(true),
            Value::False => Some(false),
            _ => None,
        }
    }

    fn as_symbol(&self) -> Option<&<Self as BasicLispValue>::Symbol> {
        match self {
            Value::Symbol(s) => Some(s),
            _ => None,
        }
    }

    fn as_pair_mut(&self) -> Option<(&mut Self, &mut Self)> {
        match self {
            Value::Pair(pair) => unsafe {
                let (car, cdr) = &mut *pair.get();
                Some((car, cdr))
            },
            _ => None,
        }
    }

    fn as_procedure(&self) -> Option<&Self::Procedure> {
        match self {
            Value::Function(y) => Some(y),
            _ => None,
        }
    }

    fn is_null(&self) -> bool {
        match self {
            Value::Nil => true,
            _ => false,
        }
    }

    fn is_number(&self) -> bool {
        match self {
            Value::Int(_) => true,
            _ => false,
        }
    }
}

impl<F> NumericLispValue for Value<F> {
    type Exact = i64;
    type Inexact = f64;

    fn int(i: i64) -> Self {
        Value::Int(i)
    }
    fn float(f: f64) -> Self {
        Value::Float(f)
    }

    fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(i) => Some(*i),
            Value::Float(f) => {
                let i = *f as i64;
                if i as f64 == *f {
                    Some(i)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn as_float(&self) -> Option<f64> {
        match self {
            Value::Int(i) => Some(*i as f64),
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }
}

impl<F> From<lexpr::Value> for Value<F> {
    fn from(lv: lexpr::Value) -> Self {
        use lexpr::Value::*;
        match lv {
            Nil | Null => Value::Nil,
            Bool(true) => Value::True,
            Bool(false) => Value::False,
            Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Int(i)
                } else if let Some(f) = n.as_f64() {
                    Value::Float(f)
                } else {
                    unimplemented!()
                }
            }
            Symbol(s) => Value::Symbol(Box::leak(s)),
            Cons(cons) => {
                let pair = cons.into_pair();
                Value::cons(pair.0, pair.1)
            }
            _ => unimplemented!(),
        }
    }
}

impl<F> PartialEq for Value<F> {
    fn eq(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Nil, Nil) | (True, True) | (False, False) => true,
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Pair(_), Pair(_)) => self.as_pair() == rhs.as_pair(),
            _ => false,
        }
    }
}

impl<F: std::fmt::Debug> std::fmt::Display for Value<F> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Undefined => write!(f, "#<UNDEFINED>"),
            Value::Nil => write!(f, "'()"),
            Value::True => write!(f, "#t"),
            Value::False => write!(f, "#f"),
            Value::Int(x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
            Value::Symbol(x) => write!(f, "{}", x),
            Value::Function(func) => write!(f, "{:?}", func),
            Value::Pair(_) => {
                let pair = self.as_pair().unwrap();
                write!(f, "({} . {})", pair.0, pair.1)
            }
        }
    }
}
