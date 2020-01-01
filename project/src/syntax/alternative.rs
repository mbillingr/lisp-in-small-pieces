use super::expression::Expression;
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
}
