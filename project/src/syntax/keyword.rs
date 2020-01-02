use super::Expression;
use crate::ast_transform::Transformer;
use crate::env::Env;
use crate::objectify::{Result, Translate};
use crate::sexpr::TrackedSexpr;
use crate::symbol::Symbol;
use crate::utils::Named;

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

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}

impl Named for MagicKeyword {
    type Name = Symbol;
    fn name(&self) -> Symbol {
        self.name
    }
}

impl PartialEq for MagicKeyword {
    fn eq(&self, other: &Self) -> bool {
        self.name.ptr_eq(&other.name) && self.handler as *const u8 == other.handler as *const u8
    }
}
