use super::expression::Expression;
use super::variable::Variable;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;

#[derive(Debug, Clone)]
pub struct FixLet {
    pub variables: Vec<Variable>,
    pub arguments: Vec<Expression>,
    pub body: Box<Expression>,
    span: SourceLocation,
}

impl FixLet {
    pub fn new(
        variables: Vec<Variable>,
        arguments: Vec<Expression>,
        body: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        FixLet {
            variables,
            arguments,
            body: body.into(),
            span,
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        self.arguments = self
            .arguments
            .into_iter()
            .map(|a| a.transform(visitor))
            .collect();
        *self.body = self.body.transform(visitor);
        self
    }
}
