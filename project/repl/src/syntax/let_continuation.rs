use super::expression::Expression;
use super::variable::LocalVariable;
use crate::ast_transform::Transformer;
use sunny_common::{impl_sourced, SourceLocation};

#[derive(Debug, Copy, Clone)]
pub enum LetContKind {
    IndefiniteContinuation,
    ExitProcedure,
}

#[derive(Debug, Clone)]
pub struct LetContinuation {
    pub kind: LetContKind,
    pub variable: LocalVariable,
    pub body: Box<Expression>,
    pub span: SourceLocation,
}

impl_sourced!(LetContinuation);

impl LetContinuation {
    pub fn new_cc(
        variable: LocalVariable,
        body: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        Self::new(LetContKind::IndefiniteContinuation, variable, body, span)
    }

    pub fn new_ep(
        variable: LocalVariable,
        body: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        Self::new(LetContKind::ExitProcedure, variable, body, span)
    }

    pub fn new(
        kind: LetContKind,
        variable: LocalVariable,
        body: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        LetContinuation {
            kind,
            variable,
            body: body.into(),
            span,
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        *self.body = self.body.transform(visitor);
        self
    }
}
