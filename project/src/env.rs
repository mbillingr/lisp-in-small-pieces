use crate::scm::Scm;
use crate::symbol::Symbol;
use crate::syntax::variable::VarDef;
use crate::syntax::{GlobalVariable, LocalVariable, Variable};
use crate::utils::Named;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env {
    locals: Vec<Variable>,
    globals: Rc<RefCell<Vec<Variable>>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            locals: vec![],
            globals: Rc::new(RefCell::new(vec![])),
        }
    }

    pub fn find_variable(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<Variable> {
        self.variables().rfind(|var| name.eq(&var.name()))
    }

    pub fn find_local(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<&LocalVariable> {
        self.locals().rfind(|var| name.eq(&var.name()))
    }

    pub fn find_global(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<GlobalVariable> {
        self.globals().rfind(|var| name.eq(&var.name()))
    }

    pub fn find_predef(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<GlobalVariable> {
        self.globals().find(|var| name.eq(&var.name()))
    }

    pub fn find_global_idx(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<usize> {
        let pos = self
            .globals
            .borrow()
            .iter()
            .rposition(|var| name.eq(&var.name()))?;
        let idx = self
            .globals
            .borrow()
            .iter()
            .take(pos)
            .filter(|var| match var {
                Variable::GlobalVariable(_) => true,
                Variable::GlobalPlaceholder(_) => true,
                _ => false,
            })
            .count();
        Some(idx)
    }

    pub fn max_global_idx(&self) -> usize {
        self.globals
            .borrow()
            .iter()
            .filter(|var| match var {
                Variable::GlobalVariable(_) => true,
                Variable::GlobalPlaceholder(_) => true,
                _ => false,
            })
            .count()
    }

    pub fn find_predef_idx(&self, name: &(impl PartialEq<Symbol> + ?Sized)) -> Option<usize> {
        self.globals()
            .enumerate()
            .find(|(_, var)| name.eq(&var.name()))
            .map(|(idx, _)| idx)
    }

    pub fn variables(&self) -> impl DoubleEndedIterator<Item = Variable> {
        self.globals
            .borrow()
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

    pub fn globals(&self) -> impl DoubleEndedIterator<Item = GlobalVariable> {
        self.globals.borrow().clone().into_iter().filter_map(|var| {
            if let Variable::GlobalVariable(gv) = var {
                Some(gv)
            } else {
                None
            }
        })
    }

    pub fn enumerate_global_names(&self) -> impl Iterator<Item = (usize, Scm)> {
        self.globals
            .borrow()
            .clone()
            .into_iter()
            .filter(|var| match var {
                Variable::GlobalVariable(_) => true,
                Variable::GlobalPlaceholder(_) => true,
                _ => false,
            })
            .enumerate()
            .filter_map(|(idx, var)| match var {
                Variable::GlobalVariable(gv) => Some((idx, Scm::Symbol(gv.name()))),
                Variable::GlobalPlaceholder(gp) => Some((idx, gp.name())),
                _ => None,
            })
    }

    pub fn ensure_global(&mut self, var: GlobalVariable) -> GlobalVariable {
        match self.find_global(&var.name()) {
            None => {
                self.globals.borrow_mut().push(var.clone().into());
                var
            }
            Some(gv) => gv,
        }
    }

    pub fn push_local(&mut self, var: impl Into<Variable>) {
        self.locals.push(var.into())
    }

    pub fn push_global(&mut self, var: impl Into<Variable>) {
        self.globals.borrow_mut().push(var.into())
    }

    pub fn extend_local<T: Into<Variable>>(&mut self, vars: impl IntoIterator<Item = T>) {
        for var in vars.into_iter() {
            self.push_local(var)
        }
    }

    pub fn extend_global<T: Into<Variable>>(&mut self, vars: impl IntoIterator<Item = T>) {
        for var in vars.into_iter() {
            self.push_global(var)
        }
    }

    pub fn drop_frame(&mut self, n: usize) {
        let n = self.locals.len() - n;
        self.locals.truncate(n);
    }

    pub fn update_global_values(&self, values: impl Iterator<Item = (Scm, Symbol)>) {
        for (gv, (value, name)) in self.globals().zip(values) {
            assert_eq!(gv.name(), name);
            match value {
                Scm::Undefined | Scm::Uninitialized => gv.ensure_value(VarDef::Unknown),
                val => gv.ensure_value(VarDef::Value(val)),
            }
        }
    }
}
