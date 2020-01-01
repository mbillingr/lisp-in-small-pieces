use super::alternative::Alternative;
use super::application::Application;
use super::assignment::Assignment;
use super::boxes::{BoxCreate, BoxRead, BoxWrite};
use super::closure::FlatClosure;
use super::constant::Constant;
use super::fixlet::FixLet;
use super::function::Function;
use super::keyword::MagicKeyword;
use super::reference::Reference;
use super::sequence::Sequence;

use crate::ast_transform::{Transformer, Visited};

sum_types! {
    #[derive(Debug, Clone)]
    pub type Expression = MagicKeyword
                        | Reference
                        | Assignment
                        | Constant
                        | Sequence
                        | Alternative
                        | Function
                        | Application
                        | FixLet
                        | BoxRead
                        | BoxWrite
                        | BoxCreate
                        | FlatClosure;
}

impl Expression {
    pub fn transform(self, visitor: &mut impl Transformer) -> Self {
        match visitor.visit(self) {
            Visited::Transformed(expr) => expr,
            Visited::Recurse(expr) => expr.default_transform(visitor),
        }
    }

    fn default_transform(self, visitor: &mut impl Transformer) -> Self {
        use Expression::*;
        match self {
            MagicKeyword(x) => x.default_transform(visitor).into(),
            Reference(x) => x.default_transform(visitor).into(),
            Assignment(x) => x.default_transform(visitor).into(),
            Constant(x) => x.default_transform(visitor).into(),
            Sequence(x) => x.default_transform(visitor).into(),
            Alternative(x) => x.default_transform(visitor).into(),
            Function(x) => x.default_transform(visitor).into(),
            Application(x) => x.default_transform(visitor).into(),
            FixLet(x) => x.default_transform(visitor).into(),
            BoxRead(x) => x.default_transform(visitor).into(),
            BoxWrite(x) => x.default_transform(visitor).into(),
            BoxCreate(x) => x.default_transform(visitor).into(),
            FlatClosure(x) => x.default_transform(visitor).into(),
        }
    }
}
