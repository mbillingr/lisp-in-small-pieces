use crate::error::Result;
use crate::scm::Scm;
use crate::sexpr::TrackedSexpr;
use crate::symbol::Symbol;
use crate::syntax::variable::VarDef;
use crate::syntax::MagicKeyword;
use std::collections::HashMap;
use std::path::PathBuf;

pub type StaticLibrary = HashMap<Symbol, ExportItem>;
pub type DynamicLibrary = HashMap<Symbol, Scm>;

#[derive(Debug, Clone)]
pub struct Library {
    pub exports: StaticLibrary,
    pub values: DynamicLibrary,
}

#[derive(Debug, Clone)]
pub enum ExportItem {
    Value(VarDef),
    Macro(MagicKeyword),
}

impl Library {
    pub fn new() -> Self {
        Library {
            exports: HashMap::new(),
            values: HashMap::new(),
        }
    }

    pub fn lookup(&self, identifier: Symbol) -> Option<&ExportItem> {
        self.exports.get(&identifier)
    }

    pub fn all_exports(&self) -> impl Iterator<Item = (Symbol, &ExportItem)> {
        self.exports.iter().map(|(s, item)| (*s, item))
    }
}

pub struct LibraryBuilder {
    lib: Library,
}

impl LibraryBuilder {
    pub fn new() -> Self {
        LibraryBuilder {
            lib: Library::new(),
        }
    }

    pub fn build(self) -> Library {
        self.lib
    }

    /*pub fn add_identifier(mut self, identifier: impl Into<Symbol>) -> Self {
        self.lib
            .exports
            .insert(identifier.into(), ExportItem::Value(VarDef::Unknown));
        self
    }*/

    pub fn add_value(mut self, identifier: impl Into<Symbol>, value: impl Into<Scm>) -> Self {
        let name = identifier.into();
        let value = value.into();
        self.lib
            .exports
            .insert(name, ExportItem::Value(VarDef::Value(value)));
        self.lib.values.insert(name, value);
        self
    }

    pub fn add_macro(mut self, identifier: impl Into<Symbol>, the_macro: MagicKeyword) -> Self {
        self.lib
            .exports
            .insert(identifier.into(), ExportItem::Macro(the_macro));
        self
    }
}

pub fn is_import(expr: &TrackedSexpr) -> bool {
    expr.car().map(|car| car == "import").unwrap_or(false)
}

pub fn libname_to_path(mut expr: &TrackedSexpr) -> Result<PathBuf> {
    let mut path = PathBuf::new();

    while expr.is_pair() {
        path.push(format!("{}", expr.car().unwrap()));
        expr = expr.cdr().unwrap();
    }

    Ok(path)
}
