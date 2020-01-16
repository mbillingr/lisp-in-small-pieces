use crate::env::Env;
use crate::scm::Scm;
use crate::symbol::Symbol;
use crate::syntax::{Variable, MagicKeyword, GlobalVariable};
use crate::sexpr::TrackedSexpr;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Library {
    exports: HashMap<Symbol, ExportItem>,
}

#[derive(Debug, Clone)]
enum ExportItem {
    Value(Scm),
    Macro(MagicKeyword),
}

impl Library {
    pub fn new() -> Self {
        Library { exports: HashMap::new() }
    }

    pub fn only(&self, identifiers: &[Symbol]) -> Self {
        unimplemented!()
    }

    pub fn except(&self, identifiers: &[Symbol]) -> Self {
        unimplemented!()
    }

    pub fn prefix(&self, prefix: impl Into<Symbol>) -> Self {
        unimplemented!()
    }

    pub fn rename(&self, mapping: impl Iterator<Item = (Symbol, Symbol)>) -> Self {
        unimplemented!()
    }

    pub fn import_into_environment(&self, env: &mut Env) {
        for (name, item) in &self.exports {
            match item {
                ExportItem::Value(x) => if env.globals.find_variable(name).is_none() { env.globals.extend(GlobalVariable::new(name)) }
                _ => unimplemented!()
            }
        }
    }
}

pub struct LibraryBuilder {
    lib: Library
}

impl LibraryBuilder {
    pub fn new() -> Self {
        LibraryBuilder {
            lib: Library::new()
        }
    }

    pub fn build(self) -> Library {
        self.lib
    }

    pub fn add_value(mut self, identifier: impl Into<Symbol>, value: impl Into<Scm>) -> Self {
        self.lib.exports.insert(identifier.into(), ExportItem::Value(value.into()));
        self
    }

    pub fn add_macro(mut self, identifier: impl Into<Symbol>, the_macro: MagicKeyword) -> Self {
        self.lib.exports.insert(identifier.into(), ExportItem::Macro(the_macro));
        self
    }
}

pub fn is_import(expr: &TrackedSexpr) -> bool {
    expr.car().map(|car| car == "import").unwrap_or(false)
}
