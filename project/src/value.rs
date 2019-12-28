use crate::ast::{RuntimePrimitive, RuntimeProcedure};
use crate::env::GlobalRuntimeEnv;
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::symbol::Symbol;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Undefined,
    Uninitialized,
    Nil,
    True,
    False,
    Int(i64),
    Float(f64),
    Symbol(Symbol),
    String(Rc<str>),
    Vector(Rc<[Value]>),

    Pair(Rc<(Value, Value)>),

    Procedure(RuntimeProcedure),
    Primitive(RuntimePrimitive),

    Box(Rc<RefCell<Value>>),
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl From<Sexpr> for Value {
    fn from(e: Sexpr) -> Self {
        match e {
            Sexpr::Nil => Value::Nil,
            Sexpr::True => Value::True,
            Sexpr::False => Value::False,
            Sexpr::Int(i) => Value::Int(i),
            Sexpr::Float(f) => Value::Float(f),
            Sexpr::Symbol(s) => Value::Symbol(s),
            Sexpr::String(s) => Value::String(s),
            Sexpr::List(l, dot) => {
                let mut x = dot.map(|d| (*d).into()).unwrap_or(Value::Nil);
                for item in l.iter().rev() {
                    x = Value::cons(item.clone().into(), x);
                }
                x
            }
            Sexpr::Vector(v) => {
                let items: Vec<Value> = v.iter().map(|i| i.clone().into()).collect();
                Value::Vector(items.into())
            }
            _ => unimplemented!(),
        }
    }
}

impl From<TrackedSexpr> for Value {
    fn from(e: TrackedSexpr) -> Self {
        e.into_sexpr().into()
    }
}

impl From<RuntimePrimitive> for Value {
    fn from(p: RuntimePrimitive) -> Self {
        Value::Primitive(p)
    }
}

impl Value {
    pub fn nil() -> Self {
        Value::Nil
    }

    pub fn bool(b: bool) -> Self {
        match b {
            true => Value::True,
            false => Value::False,
        }
    }

    pub fn cons(a: Value, b: Value) -> Self {
        Value::Pair(Rc::new((a, b)))
    }

    pub fn boxed(x: Value) -> Value {
        Value::Box(Rc::new(RefCell::new(x)))
    }

    pub fn is_true(&self) -> bool {
        match self {
            Value::False => false,
            _ => true,
        }
    }

    pub fn is_bool(&self) -> bool {
        self.as_bool().is_some()
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::True => Some(true),
            Value::False => Some(false),
            _ => None,
        }
    }

    pub fn is_pair(&self) -> bool {
        self.as_pair().is_some()
    }

    pub fn as_pair(&self) -> Option<&Rc<(Value, Value)>> {
        match self {
            Value::Pair(p) => Some(p),
            _ => None,
        }
    }

    pub fn invoke(&self, args: Vec<Value>, sg: &mut GlobalRuntimeEnv) -> Value {
        match self {
            Value::Procedure(proc) => proc.invoke(args, sg),
            //Value::Primitive(proc) => proc.invoke(args),
            _ => panic!("Cannot invoke {:?}", self),
        }
    }

    pub fn is_eq(&self, other: &Self) -> bool {
        match [self, other] {
            [Value::Undefined, Value::Undefined] => false,
            [Value::Uninitialized, Value::Uninitialized] => true,
            [Value::Nil, Value::Nil] => true,
            [Value::True, Value::True] => true,
            [Value::False, Value::False] => true,
            [Value::Int(a), Value::Int(b)] => a == b,
            [Value::Float(a), Value::Float(b)] => a == b,
            [Value::Symbol(a), Value::Symbol(b)] => Symbol::ptr_eq(a, b),
            [Value::String(a), Value::String(b)] => Rc::ptr_eq(a, b),
            [Value::Vector(a), Value::Vector(b)] => Rc::ptr_eq(a, b),
            [Value::Pair(a), Value::Pair(b)] => Rc::ptr_eq(a, b),
            [Value::Procedure(a), Value::Procedure(b)] => a as *const _ == b as *const _,
            [Value::Primitive(a), Value::Primitive(b)] => a as *const _ == b as *const _,
            [Value::Box(a), Value::Box(b)] => Rc::ptr_eq(a, b),
            [_, _] => false,
        }
    }

    pub fn set(&self, value: Value) -> Value {
        match self {
            Value::Box(target) => target.replace(value),
            _ => panic!("set on unboxed value"),
        }
    }

    pub fn get(&self) -> Value {
        match self {
            Value::Box(target) => target.borrow().clone(),
            _ => panic!("set on unboxed value"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Value::*;
        match self {
            Undefined => Ok(()),
            Uninitialized => write!(f, "*uninitialized*"),
            Nil => write!(f, "'()"),
            True => write!(f, "#t"),
            False => write!(f, "#f"),
            Int(i) => write!(f, "{}", i),
            Float(i) => write!(f, "{}", i),
            Symbol(s) => write!(f, "{}", s),
            String(s) => write!(f, "\"{}\"", s),
            Pair(pair) => {
                let mut pair = pair;
                write!(f, "(")?;
                write!(f, "{}", pair.0)?;
                while let Pair(ref p) = pair.1 {
                    pair = p;
                    write!(f, " {}", pair.0)?;
                }
                if let Nil = pair.1 {
                    write!(f, ")")
                } else {
                    write!(f, " . {})", pair.1)
                }
            }
            Vector(items) => {
                write!(f, "#(")?;
                write!(f, "{}", items[0])?;
                for i in &items[1..] {
                    write!(f, " {}", i)?;
                }
                write!(f, ")")
            }
            Procedure(proc) => write!(f, "{}", proc),
            Primitive(proc) => write!(f, "{}", proc),
            Box(x) => write!(f, "[{}]", x.borrow()),
        }
    }
}
