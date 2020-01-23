use crate::symbol::Symbol;
use crate::syntax::variable::SyntacticBinding;
use crate::syntax::{GlobalVariable, LocalVariable, MagicKeyword, Variable};
use crate::utils::Named;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Env {
    locals: Vec<Variable>,
    globals: Vec<Variable>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            locals: vec![],
            globals: vec![],
        }
    }

    pub fn find_variable(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<Variable> {
        self.variables().rfind(|var| name.eq(&var.name()))
    }

    pub fn find_local(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<&LocalVariable> {
        self.locals().rfind(|var| name.eq(&var.name()))
    }

    pub fn find_global(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<&GlobalVariable> {
        self.globals().rfind(|var| name.eq(&var.name()))
    }

    pub fn find_predef(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<&GlobalVariable> {
        self.globals().find(|var| name.eq(&var.name()))
    }

    pub fn find_global_idx(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<usize> {
        let pos = self.globals.iter().rposition(|var| name.eq(&var.name()))?;
        let idx = self
            .globals
            .iter()
            .take(pos)
            .filter_map(|var| {
                if let Variable::GlobalVariable(gv) = var {
                    Some(gv)
                } else {
                    None
                }
            })
            .count();
        Some(idx)
    }

    pub fn find_predef_idx(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<usize> {
        self.globals()
            .enumerate()
            .find(|(_, var)| name.eq(&var.name()))
            .map(|(idx, _)| idx)
    }

    /*pub fn find_syntax_bound(
        &self,
        name: &(impl PartialEq<Symbol> + ?Sized),
    ) -> Option<SyntacticBinding> {
        self.syntax.find_variable(name)
    }*/

    pub fn variables(&self) -> impl DoubleEndedIterator<Item = Variable> {
        self.globals
            .clone()
            .into_iter()
            .chain(self.locals.clone().into_iter())
    }

    pub fn locals(&self) -> impl DoubleEndedIterator<Item = &LocalVariable> {
        self.locals.iter().filter_map(|var| {
            if let Variable::LocalVariable(gv) = var {
                Some(gv)
            } else {
                None
            }
        })
    }

    pub fn globals(&self) -> impl DoubleEndedIterator<Item = &GlobalVariable> {
        self.globals.iter().filter_map(|var| {
            if let Variable::GlobalVariable(gv) = var {
                Some(gv)
            } else {
                None
            }
        })
    }

    pub fn ensure_global(&mut self, var: GlobalVariable) {
        if self.find_global(&var.name()).is_none() {
            self.globals.push(var.into())
        }
    }

    pub fn push_local(&mut self, var: impl Into<Variable>) {
        self.locals.push(var.into())
    }

    pub fn push_global(&mut self, var: impl Into<Variable>) {
        self.globals.push(var.into())
    }

    pub fn extend_local<T: Into<Variable>>(&mut self, vars: impl IntoIterator<Item = T>) {
        for var in vars.into_iter() {
            self.push_local(var)
        }
    }

    pub fn extend_global<T: Into<Variable>>(&mut self, vars: impl IntoIterator<Item = T>) {
        for var in vars.into_iter() {
            self.push_local(var)
        }
    }

    /*pub fn extend_syntax(&mut self, vars: impl IntoIterator<Item = SyntacticBinding>) {
        self.syntax.extend_frame(vars.into_iter())
    }

    pub fn drop_syntax(&mut self, n: usize) {
        self.syntax.pop_frame(n);
    }*/

    pub fn drop_frame(&mut self, n: usize) {
        let n = self.locals.len() - n;
        self.locals.truncate(n);
    }

    pub fn deep_clone(&self) -> Self {
        Env {
            locals: self.locals.clone(),
            globals: self.globals.clone(),
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
        find_variable(self.0.as_slice(), name).cloned()
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

fn find_variable<'a, V: Clone + Named>(
    env: &'a [V],
    name: &(impl PartialEq<V::Name> + ?Sized),
) -> Option<&'a V> {
    env.iter().rev().find(|var| name.eq(&var.name()))
}

fn find_original_variable<'a, V: Clone + Named>(
    env: &'a [V],
    name: &(impl PartialEq<V::Name> + ?Sized),
) -> Option<&'a V> {
    env.iter().find(|var| name.eq(&var.name()))
}
