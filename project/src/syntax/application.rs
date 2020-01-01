use super::expression::Expression;
use super::variable::PredefinedVariable;
use crate::source::SourceLocation;

sum_type! {
    #[derive(Debug, Clone)]
    pub type Application = RegularApplication | PredefinedApplication;
}

#[derive(Debug, Clone)]
pub struct RegularApplication {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    span: SourceLocation,
}

impl RegularApplication {
    pub fn new(
        function: impl Into<Box<Expression>>,
        arguments: Vec<Expression>,
        span: SourceLocation,
    ) -> Self {
        RegularApplication {
            function: function.into(),
            arguments,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PredefinedApplication {
    pub variable: PredefinedVariable,
    pub arguments: Vec<Expression>,
    span: SourceLocation,
}

impl PredefinedApplication {
    pub fn new(
        variable: PredefinedVariable,
        arguments: Vec<Expression>,
        span: SourceLocation,
    ) -> Self {
        PredefinedApplication {
            variable,
            arguments,
            span,
        }
    }
}
