use super::Expression;
use crate::env::Env;
use crate::objectify::{Result, Translate};
use crate::sexpr::TrackedSexpr;
use crate::symbol::Symbol;

pub type MagicKeywordHandler = fn(&mut Translate, &TrackedSexpr, &Env) -> Result<Expression>;

#[derive(Clone)]
pub struct MagicKeyword {
    pub name: Symbol,
    pub handler: MagicKeywordHandler,
}

impl std::fmt::Debug for MagicKeyword {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl MagicKeyword {
    pub fn new(name: impl Into<Symbol>, handler: MagicKeywordHandler) -> Self {
        MagicKeyword {
            name: name.into(),
            handler,
        }
    }

    pub fn name(&self) -> &Symbol {
        &self.name
    }
}

impl PartialEq for MagicKeyword {
    fn eq(&self, other: &Self) -> bool {
        self.name.ptr_eq(&other.name) && self.handler as *const u8 == other.handler as *const u8
    }
}
