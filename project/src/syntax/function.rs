use super::variable::{LocalVariable, Variable};
use super::expression::Expression;
use crate::source::SourceLocation;
use crate::ast::Arity;
use std::convert::TryInto;

#[derive(Debug, Clone)]
pub struct Function {
    pub variables: Vec<Variable>,
    pub body: Box<Expression>,
    span: SourceLocation,
}

impl Function {
    pub fn new(variables: Vec<Variable>, body: impl Into<Box<Expression>>, span: SourceLocation) -> Self {
        Function {
            variables,
            body: body.into(),
            span,
        }
    }

    pub fn arity(&self) -> Arity {
        if self
            .variables
            .last()
            .and_then(|arg| arg.try_into().ok())
            .map(LocalVariable::is_dotted)
            .unwrap_or(false)
        {
            Arity::AtLeast(self.variables.len() as u16 - 1)
        } else {
            Arity::Exact(self.variables.len() as u16)
        }
    }
}
