use super::expression::Expression;
use crate::ast_transform::Transformer;
use crate::syntax::NoOp;
use sunny_common::{impl_sourced, SourceLocation, SourceLocation::NoSource};

#[derive(Debug, Clone)]
pub struct Sequence {
    pub first: Box<Expression>,
    pub next: Box<Expression>,
    span: SourceLocation,
}

impl_sourced!(Sequence);

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

    pub fn append(&mut self, other: Expression) {
        match &mut *self.next {
            Expression::Sequence(s) => s.append(other),
            Expression::NoOp(_) => self.next = Box::new(other),
            _ => {
                self.next = Box::new(
                    Sequence::new(
                        std::mem::replace(&mut self.next, Box::new(Expression::NoOp(NoOp))),
                        other,
                        NoSource,
                    )
                    .into(),
                );
            }
        }
    }
}
