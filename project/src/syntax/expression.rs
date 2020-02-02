use super::alternative::Alternative;
use super::application::Application;
use super::assignment::Assignment;
use super::boxes::{BoxCreate, BoxRead, BoxWrite};
use super::closure::FlatClosure;
use super::constant::Constant;
use super::definition::GlobalDefine;
use super::fixlet::FixLet;
use super::function::Function;
use super::import::Import;
use super::keyword::MagicKeyword;
use super::noop::NoOp;
use super::reference::Reference;
use super::sequence::Sequence;

use crate::ast_transform::{Transformer, Visited};
use crate::source::SourceLocation;
use crate::utils::Sourced;

sum_types! {
    #[derive(Debug, Clone)]
    pub type Expression = NoOp
                        | MagicKeyword
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
                        | FlatClosure
                        | GlobalDefine;
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
            GlobalDefine(x) => x.default_transform(visitor).into(),
            NoOp(x) => x.default_transform(visitor).into(),
        }
    }
}

impl Sourced for Expression {
    fn source(&self) -> &SourceLocation {
        use Expression::*;
        match self {
            MagicKeyword(_) => &SourceLocation::NoSource,
            Reference(x) => x.source(),
            Assignment(x) => x.source(),
            Constant(x) => x.source(),
            Sequence(x) => x.source(),
            Alternative(x) => x.source(),
            Application(x) => x.source(),
            Function(x) => x.source(),
            FixLet(x) => x.source(),
            BoxRead(x) => x.source(),
            BoxWrite(x) => x.source(),
            BoxCreate(x) => x.source(),
            FlatClosure(x) => x.source(),
            GlobalDefine(x) => x.source(),
            NoOp(x) => x.source(),
        }
    }
}
