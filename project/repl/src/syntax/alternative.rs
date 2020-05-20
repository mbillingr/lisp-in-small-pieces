use super::expression::Expression;
use crate::ast_transform::Transformer;
use crate::scm::Scm;
use crate::syntax::Reify;
use sunny_common::{impl_sourced, SourceLocation};

#[derive(Debug, Clone)]
pub struct Alternative {
    pub condition: Box<Expression>,
    pub consequence: Box<Expression>,
    pub alternative: Box<Expression>,
    span: SourceLocation,
}

impl_sourced!(Alternative);

impl Alternative {
    pub fn new(
        condition: impl Into<Box<Expression>>,
        consequence: impl Into<Box<Expression>>,
        alternative: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        Alternative {
            condition: condition.into(),
            consequence: consequence.into(),
            alternative: alternative.into(),
            span,
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        *self.condition = self.condition.transform(visitor);
        *self.consequence = self.consequence.transform(visitor);
        *self.alternative = self.alternative.transform(visitor);
        self
    }
}

impl Reify for Alternative {
    fn reify(&self) -> Scm {
        Scm::list(vec![
            Scm::symbol("if"),
            self.condition.reify(),
            self.consequence.reify(),
            self.alternative.reify(),
        ])
    }
}
