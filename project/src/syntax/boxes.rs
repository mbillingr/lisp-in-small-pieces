use super::expression::Expression;
use super::reference::Reference;
use super::variable::Variable;
use crate::source::SourceLocation;

#[derive(Debug, Clone)]
pub struct BoxRead {
    pub reference: Reference,
    span: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct BoxWrite {
    pub reference: Reference,
    pub form: Box<Expression>,
    span: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct BoxCreate {
    pub variable: Variable,
    span: SourceLocation,
}

impl BoxRead {
    pub fn new(reference: Reference, span: SourceLocation) -> Self {
        BoxRead { reference, span }
    }
}

impl BoxWrite {
    pub fn new(
        reference: Reference,
        form: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        BoxWrite {
            reference,
            form: form.into(),
            span,
        }
    }
}

impl BoxCreate {
    pub fn new(variable: Variable, span: SourceLocation) -> Self {
        BoxCreate { variable, span }
    }
}
