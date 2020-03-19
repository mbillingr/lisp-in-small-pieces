use super::Expression;
use crate::ast_transform::Transformer;
use crate::error::Result;
use crate::objectify::Translate;
use crate::sexpr::TrackedSexpr;
use crate::symbol::Symbol;
use crate::utils::Named;
use std::rc::Rc;

#[derive(Clone)]
pub struct MagicKeyword {
    pub name: Symbol,
    pub handler: MagicKeywordHandler,
}

#[derive(Clone)]
pub struct MagicKeywordHandler(Rc<Rc<dyn Fn(&mut Translate, &TrackedSexpr) -> Result<Expression>>>);

impl MagicKeywordHandler {
    pub fn new(
        handler: impl Fn(&mut Translate, &TrackedSexpr) -> Result<Expression> + 'static,
    ) -> Self {
        MagicKeywordHandler(Rc::new(Rc::new(handler)))
    }

    pub fn invoke(&self, trans: &mut Translate, sexpr: &TrackedSexpr) -> Result<Expression> {
        (self.0)(trans, sexpr)
    }

    pub unsafe fn replace(&self, other: Self) {
        let tmp = &*self.0 as *const _ as *mut _;
        *tmp = (*other.0).clone();
    }
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
            handler: MagicKeywordHandler::new(handler),
        }
    }

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }

    pub fn renamed(&self, newname: Symbol) -> MagicKeyword {
        MagicKeyword {
            name: newname,
            handler: self.handler.clone(),
        }
    }

    pub unsafe fn replace_handler(&self, handler: MagicKeywordHandler) {
        self.handler.replace(handler)
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
        self.name.ptr_eq(&other.name) && self.handler == other.handler
    }
}

impl PartialEq for MagicKeywordHandler {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
