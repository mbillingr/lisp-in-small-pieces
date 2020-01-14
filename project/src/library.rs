use crate::syntax::Variable;
use crate::env::Env;
use crate::scm::Scm;
use crate::symbol::Symbol;

pub struct Library {
    exports: Vec<(Variable, Scm)>
}

impl Library {
    pub fn new() -> Self {
        Library {
            exports: vec![]
        }
    }

    fn only(&self, identifiers: &[Symbol]) -> Self {
        unimplemented!()
    }

    fn except(&self, identifiers: &[Symbol]) -> Self {
        unimplemented!()
    }

    fn prefix(&self, prefix: impl Into<Symbol>) -> Self {
        unimplemented!()
    }

    fn rename(&self, mapping: impl Iterator<Item=(Symbol, Symbol)>) -> Self {
        unimplemented!()
    }

    fn import_into_environment(&self, env: &mut Env) {
        unimplemented!()
    }
}
