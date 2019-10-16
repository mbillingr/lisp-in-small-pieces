use crate::types::{ActivationFrame, Closure, Escape, Primitive, Scm, Symbol};
use std::any::{Any, TypeId};
use std::fmt::{Debug, Display};

pub trait UserValue: Debug + Display + 'static {
    fn type_id(&self) -> TypeId;
}

impl<T: Debug + Display + 'static> UserValue for T {
    fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }
}

impl dyn UserValue {
    pub fn as_scm(&'static self) -> Scm {
        Scm::from_value(ScmBoxedValue::UserValue(self))
    }

    pub fn is<T: UserValue>(&self) -> bool {
        TypeId::of::<T>() == self.type_id()
    }

    pub fn downcast_ref<T: UserValue>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe { Some(&*(self as *const dyn UserValue as *const T)) }
        } else {
            None
        }
    }
}

#[derive(Copy, Clone)]
pub enum ScmBoxedValue {
    Symbol(Symbol),
    String(&'static String),

    Primitive(&'static Primitive),
    Frame(&'static ActivationFrame),
    Closure(&'static Closure),
    Escape(&'static Escape),

    UserValue(&'static dyn UserValue),
}

impl ScmBoxedValue {
    fn from_user_value<T: UserValue>(value: T) -> Self {
        ScmBoxedValue::UserValue(Box::leak(Box::new(value)))
    }

    pub fn as_symbol(&self) -> Option<Symbol> {
        match self {
            ScmBoxedValue::Symbol(s) => Some(s),
            _ => None,
        }
    }
    pub fn as_string(&self) -> Option<&'static String> {
        match self {
            ScmBoxedValue::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_primitive(&self) -> Option<&'static Primitive> {
        match self {
            ScmBoxedValue::Primitive(p) => Some(p),
            _ => None,
        }
    }

    pub fn as_frame(&self) -> Option<&'static ActivationFrame> {
        match self {
            ScmBoxedValue::Frame(frame) => Some(frame),
            _ => None,
        }
    }

    pub fn as_closure(&self) -> Option<&'static Closure> {
        match self {
            ScmBoxedValue::Closure(cls) => Some(cls),
            _ => None,
        }
    }

    pub fn as_escape(&self) -> Option<&'static Escape> {
        match self {
            ScmBoxedValue::Escape(esc) => Some(esc),
            _ => None,
        }
    }

    pub fn as_user_value<T: UserValue>(&self) -> Option<&'static T> {
        match self {
            ScmBoxedValue::UserValue(val) => dbg!(val).downcast_ref::<T>(),
            _ => None,
        }
    }
}

impl PartialEq for ScmBoxedValue {
    fn eq(&self, rhs: &Self) -> bool {
        use ScmBoxedValue::*;
        match (self, rhs) {
            (Symbol(a), Symbol(b)) => a == b,
            (String(a), String(b)) => a == b,
            _ => false,
        }
    }
}
