use crate::ast::{RuntimePrimitive, Variable};
use crate::symbol::Symbol;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type Env = Rc<Environment>;

#[derive(Debug)]
pub enum Environment {
    Empty,
    Entry(Env, Variable),
    GlobalMarker(RefCell<(Env, usize)>),
}

impl EnvAccess for Env {
    fn new_empty() -> Self {
        Rc::new(Environment::Empty).mark_global(0)
    }

    fn update_runtime_globals(&self, sg: &mut GlobalRuntimeEnv) {
        match &**self {
            Environment::GlobalMarker(next) => next.borrow().0.update_runtime_globals(sg),
            Environment::Entry(next, var) => {
                match var {
                    Variable::Global(name) => sg.ensure_global(name.clone()),
                    _ => {}
                }
                next.update_runtime_globals(sg)
            }
            Environment::Empty => {}
        }
    }

    fn find_variable(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<Variable> {
        match &**self {
            Environment::Empty => None,
            Environment::GlobalMarker(env) => env.borrow().0.find_variable(name),
            Environment::Entry(env, var) => {
                if name.eq(var.name()) {
                    Some(var.clone())
                } else {
                    env.find_variable(name)
                }
            }
        }
    }

    fn find_global_environment(&self) -> &Env {
        match &**self {
            Environment::GlobalMarker(_) => self,
            Environment::Entry(env, _) => env.find_global_environment(),
            Environment::Empty => panic!("No global environment"),
        }
    }

    fn extend(self, var: Variable) -> Env {
        Rc::new(Environment::Entry(self, var))
    }

    fn extend_frame(mut self, vars: impl Iterator<Item = Variable>) -> Self {
        for var in vars {
            self = self.extend(var);
        }
        self
    }

    fn insert_global(&self, var: Variable) {
        if let Environment::GlobalMarker(env) = &**self.find_global_environment() {
            let r = env.borrow().0.clone().extend(var);
            let mut e = env.borrow_mut();
            e.0 = r;
            e.1 += 1;
        } else {
            unreachable!()
        }
    }

    fn mark_global(self, n: usize) -> Self {
        Rc::new(Environment::GlobalMarker(RefCell::new((self, n))))
    }
}

pub trait EnvAccess {
    fn new_empty() -> Self;
    fn update_runtime_globals(&self, sr: &mut GlobalRuntimeEnv);
    fn find_variable(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<Variable>;
    fn find_global_environment(&self) -> &Self;
    fn extend(self, var: Variable) -> Self;
    fn extend_frame(self, vars: impl Iterator<Item = Variable>) -> Self;
    fn insert_global(&self, var: Variable);
    fn mark_global(self, n: usize) -> Self;
}

pub type LexicalRuntimeEnv = Rc<EnvChain>;

#[derive(Debug)]
pub struct EnvChain {
    var: Symbol,
    val: Value,
    next: Option<LexicalRuntimeEnv>,
}

impl EnvChain {
    pub fn new() -> Rc<Self> {
        Rc::new(EnvChain {
            var: ".".into(),
            val: Value::Undefined,
            next: None,
        })
    }

    pub fn extend(self: Rc<Self>, var: Symbol, val: Value) -> Rc<Self> {
        Rc::new(EnvChain {
            var,
            val: val,
            next: Some(self),
        })
    }

    pub fn get_lexical(&self, name: &Symbol) -> Value {
        if self.var == *name {
            self.val.clone()
        } else {
            match self.next {
                Some(ref env) => env.get_lexical(name),
                None => panic!("Unbound lexical variable: {}", name),
            }
        }
    }

    pub unsafe fn set_lexical(&self, name: &Symbol, value: Value) {
        if self.var == *name {
            *(&self.val as *const _ as *mut _) = value;
        } else {
            match self.next {
                Some(ref env) => env.set_lexical(name, value),
                None => panic!("Unbound lexical variable: {}", name),
            }
        }
    }
}

pub struct GlobalRuntimeEnv {
    predef: HashMap<Symbol, RuntimePrimitive>,
    globals: HashMap<Symbol, RefCell<Value>>,
}

impl GlobalRuntimeEnv {
    pub fn new(predef: HashMap<Symbol, RuntimePrimitive>) -> Self {
        GlobalRuntimeEnv {
            predef,
            globals: HashMap::new(),
        }
    }

    pub fn get_predefined(&self, name: &Symbol) -> &RuntimePrimitive {
        &self.predef[name]
    }

    pub fn get_global(&self, name: &Symbol) -> Value {
        self.globals[name].borrow().clone()
    }

    pub fn set_global(&self, name: &Symbol, val: Value) {
        *self.globals[name].borrow_mut() = val;
    }

    pub fn ensure_global(&mut self, name: Symbol) {
        self.globals
            .entry(name)
            .or_insert_with(|| RefCell::new(Value::Uninitialized));
    }
}
