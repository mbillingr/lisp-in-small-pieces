use crate::ast::{RuntimePrimitive, Variable};
use crate::symbol::Symbol;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env {
    pub locals: Environment,
    pub globals: Environment,
    pub predef: Environment,
}

impl Env {
    pub fn new() -> Self {
        Env {
            locals: Environment::new(),
            globals: Environment::new(),
            predef: Environment::new(),
        }
    }

    /*fn update_runtime_globals(&self, sg: &mut GlobalRuntimeEnv) {
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
    }*/

    pub fn find_variable(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<Variable> {
        self.locals
            .find_variable(name)
            .or_else(|| self.globals.find_variable(name))
            .or_else(|| self.predef.find_variable(name))
    }
}

#[derive(Debug, Clone)]
pub struct Environment(Rc<RefCell<Vec<Variable>>>);

impl Environment {
    pub fn new() -> Self {
        Environment(Rc::new(RefCell::new(vec![])))
    }

    pub fn len(&self) -> usize {
        self.0.borrow().len()
    }

    pub fn find_variable(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<Variable> {
        self.0
            .borrow()
            .iter()
            .rev()
            .find(|var| name.eq(var.name()))
            .cloned()
    }

    pub fn find_idx(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<usize> {
        self.0
            .borrow()
            .iter()
            .enumerate()
            .rev()
            .find(|(_, var)| name.eq(var.name()))
            .map(|(idx, _)| idx)
    }

    pub fn extend_frame(&self, vars: impl Iterator<Item = Variable>) {
        self.0.borrow_mut().extend(vars)
    }

    pub fn extend(&self, var: Variable) {
        self.0.borrow_mut().push(var);
    }

    pub fn pop_frame(&self, n: usize) {
        let mut vars = self.0.borrow_mut();
        let n = vars.len() - n;
        vars.truncate(n);
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
