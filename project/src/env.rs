use crate::ast::{RuntimePrimitive, Variable};
use crate::symbol::Symbol;
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
