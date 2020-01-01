use super::expression::Expression;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;

#[derive(Debug, Clone)]
pub struct Sequence {
    pub first: Box<Expression>,
    pub next: Box<Expression>,
    span: SourceLocation,
}

impl Sequence {
    pub fn new(
        first: impl Into<Box<Expression>>,
        next: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        Sequence {
            first: first.into(),
            next: next.into(),
            span,
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        *self.first = self.first.transform(visitor);
        *self.next = self.next.transform(visitor);
        self
    }
}
