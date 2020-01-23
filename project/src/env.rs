use crate::symbol::Symbol;
use crate::syntax::variable::SyntacticBinding;
use crate::syntax::{GlobalVariable, LocalVariable, MagicKeyword, Variable};
use crate::utils::Named;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Env {
    locals: Environment<LocalVariable>,
    globals: Environment<GlobalVariable>,
    macros: Environment<MagicKeyword>,
    syntax: Environment<SyntacticBinding>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            locals: Environment::new(),
            globals: Environment::new(),
            macros: Environment::new(),
            syntax: Environment::new(),
        }
    }

    pub fn find_variable(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<Variable> {
        self.locals
            .find_variable(name)
            .map(Variable::from)
            .or_else(|| self.globals.find_variable(name).map(Variable::from))
            .or_else(|| self.macros.find_variable(name).map(Variable::from))
    }

    pub fn variables(&self) -> impl Iterator<Item = Variable> {
        self.macros
            .iter()
            .map(Variable::from)
            .chain(self.globals.iter().map(Variable::from))
            .chain(self.locals.iter().map(Variable::from))
    }

    pub fn find_predef(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<GlobalVariable> {
        self.globals.find_original_variable(name)
    }

    pub fn find_global_idx(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<usize> {
        self.globals.find_idx(name)
    }

    pub fn find_predef_idx(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<usize> {
        self.globals.find_original_idx(name)
    }

    pub fn find_syntax_bound(
        &self,
        name: &(impl PartialEq<Symbol> + ?Sized),
    ) -> Option<SyntacticBinding> {
        self.syntax.find_variable(name)
    }

    pub fn globals(&self) -> impl Iterator<Item = GlobalVariable> {
        self.globals.iter()
        //.filter_map(|var| if let Variable::GlobalVariable(gv) = var {Some(gv)} else {None})
    }

    pub fn ensure_global(&mut self, var: GlobalVariable) {
        self.globals.ensure_variable(var)
    }

    pub fn push(&mut self, var: impl Into<Variable>) {
        match var.into() {
            Variable::LocalVariable(v) => self.locals.extend(v),
            Variable::GlobalVariable(v) => self.globals.extend(v),
            Variable::MagicKeyword(v) => self.macros.extend(v),
            Variable::FreeVariable(_) => panic!("There is no environment for free variables"),
            Variable::SyntacticBinding(v) => self.syntax.extend(v),
        }
    }

    pub fn extend<T: Into<Variable>>(&mut self, vars: impl IntoIterator<Item = T>) {
        for var in vars.into_iter() {
            self.push(var)
        }
    }

    pub fn extend_syntax(&mut self, vars: impl IntoIterator<Item = SyntacticBinding>) {
        self.syntax.extend_frame(vars.into_iter())
    }

    pub fn drop_syntax(&mut self, n: usize) {
        self.syntax.pop_frame(n);
    }

    pub fn drop_frame(&mut self, n: usize) {
        self.locals.pop_frame(n)
    }

    pub fn deep_clone(&self) -> Self {
        Env {
            locals: self.locals.deep_clone(),
            globals: self.globals.deep_clone(),
            macros: self.macros.deep_clone(),
            syntax: self.syntax.deep_clone(),
        }
    }
}

#[derive(Debug)]
pub struct Environment<V>(Vec<V>);

impl<V: Clone + Named> Environment<V> {
    pub fn new() -> Self {
        Environment(vec![])
    }

    /*pub fn len(&self) -> usize {
        self.0.borrow().len()
    }*/

    pub fn find_variable(&self, name: &(impl PartialEq<V::Name> + ?Sized)) -> Option<V> {
        self.0
            .iter()
            .rev()
            .find(|var| name.eq(&var.name()))
            .cloned()
    }

    pub fn find_idx(&self, name: &(impl PartialEq<V::Name> + ?Sized)) -> Option<usize> {
        self.0
            .iter()
            .enumerate()
            .rev()
            .find(|(_, var)| name.eq(&var.name()))
            .map(|(idx, _)| idx)
    }

    pub fn find_original_variable(&self, name: &(impl PartialEq<V::Name> + ?Sized)) -> Option<V> {
        self.0.iter().find(|var| name.eq(&var.name())).cloned()
    }

    pub fn find_original_idx(&self, name: &(impl PartialEq<V::Name> + ?Sized)) -> Option<usize> {
        self.0
            .iter()
            .enumerate()
            .find(|(_, var)| name.eq(&var.name()))
            .map(|(idx, _)| idx)
    }

    pub fn ensure_variable(&mut self, var: V) {
        if self.find_variable(&var.name()).is_none() {
            self.extend(var)
        }
    }

    /*pub fn at(&self, idx: usize) -> V {
        self.0.borrow()[idx].clone()
    }*/

    pub fn extend_frame(&mut self, vars: impl Iterator<Item = V>) {
        self.0.extend(vars)
    }

    pub fn extend(&mut self, var: V) {
        self.0.push(var);
    }

    pub fn pop_frame(&mut self, n: usize) {
        let n = self.0.len() - n;
        self.0.truncate(n);
    }

    pub fn deep_clone(&self) -> Self {
        Environment(self.0.clone())
    }

    pub fn iter(&self) -> impl Iterator<Item = V> {
        self.0.clone().into_iter()
    }
}
