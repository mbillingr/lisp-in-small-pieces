use super::Expression;
use crate::ast_transform::Transformer;
use crate::error::Result;
use crate::objectify::Translate;
use crate::sexpr::TrackedSexpr;
use crate::symbol::Symbol;
use crate::utils::Named;
use std::rc::Rc;

pub type MagicKeywordHandler = Rc<dyn Fn(&mut Translate, &TrackedSexpr) -> Result<Expression>>;

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
    pub fn new(
        name: impl Into<Symbol>,
        handler: impl Fn(&mut Translate, &TrackedSexpr) -> Result<Expression> + 'static,
    ) -> Self {
        MagicKeyword {
            name: name.into(),
            handler: Rc::new(handler),
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
        self.name.ptr_eq(&other.name) && Rc::ptr_eq(&self.handler, &other.handler)
    }
}
