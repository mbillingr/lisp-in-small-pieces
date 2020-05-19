use super::expression::Expression;
use crate::ast_transform::Transformer;
use crate::scm::Scm;
use crate::syntax::{NoOp, Reify};
use sunny_parser::SourceLocation;
use sunny_parser::SourceLocation::NoSource;

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

impl Reify for Sequence {
    fn reify(&self) -> Scm {
        let mut items = vec![Scm::symbol("begin"), self.first.reify()];
        let mut current = &*self.next;
        while let Expression::Sequence(s) = current {
            items.push(s.first.reify());
            current = &*s.next;
        }
        items.push(current.reify());
        Scm::list(items)
    }
}
