use super::expression::Expression;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;

#[derive(Debug, Clone)]
pub struct Alternative {
    pub condition: Box<Expression>,
    pub consequence: Box<Expression>,
    pub alternative: Box<Expression>,
    span: SourceLocation,
}

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
