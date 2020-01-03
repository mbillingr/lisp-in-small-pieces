use crate::symbol::Symbol;
use crate::syntax::{GlobalVariable, LocalVariable, MagicKeyword, PredefinedVariable, Variable};
use crate::utils::Named;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env {
    pub locals: Environment<LocalVariable>,
    pub globals: Environment<GlobalVariable>,
    pub predef: Environment<PredefinedVariable>,
    pub macros: Environment<MagicKeyword>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            locals: Environment::new(),
            globals: Environment::new(),
            predef: Environment::new(),
            macros: Environment::new(),
        }
    }

    pub fn find_variable(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<Variable> {
        self.locals
            .find_variable(name)
            .map(Variable::from)
            .or_else(|| self.globals.find_variable(name).map(Variable::from))
            .or_else(|| self.predef.find_variable(name).map(Variable::from))
            .or_else(|| self.macros.find_variable(name).map(Variable::from))
    }
}

#[derive(Debug, Clone)]
pub struct Environment<V>(Rc<RefCell<Vec<V>>>);

impl<V: Clone + Named> Environment<V> {
    pub fn new() -> Self {
        Environment(Rc::new(RefCell::new(vec![])))
    }

    pub fn len(&self) -> usize {
        self.0.borrow().len()
    }

    pub fn find_variable(&self, name: &(impl PartialEq<V::Name> + ?Sized)) -> Option<V> {
        self.0
            .borrow()
            .iter()
            .rev()
            .find(|var| name.eq(&var.name()))
            .cloned()
    }

    pub fn find_idx(&self, name: &(impl PartialEq<V::Name> + ?Sized)) -> Option<usize> {
        self.0
            .borrow()
            .iter()
            .enumerate()
            .rev()
            .find(|(_, var)| name.eq(&var.name()))
            .map(|(idx, _)| idx)
    }

    pub fn extend_frame(&self, vars: impl Iterator<Item = V>) {
        self.0.borrow_mut().extend(vars)
    }

    pub fn extend(&self, var: V) {
        self.0.borrow_mut().push(var);
    }

    pub fn pop_frame(&self, n: usize) {
        let mut vars = self.0.borrow_mut();
        let n = vars.len() - n;
        vars.truncate(n);
    }
}
