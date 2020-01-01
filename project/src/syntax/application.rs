use super::expression::Expression;
use super::variable::PredefinedVariable;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;

sum_type! {
    #[derive(Debug, Clone)]
    pub type Application = RegularApplication | PredefinedApplication;
}

impl Application {
    pub fn default_transform(self, visitor: &mut impl Transformer) -> Self {
        use Application::*;
        match self {
            RegularApplication(x) => x.default_transform(visitor).into(),
            PredefinedApplication(x) => x.default_transform(visitor).into(),
        }
    }
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

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        *self.function = self.function.transform(visitor);
        self.arguments = self
            .arguments
            .into_iter()
            .map(|a| a.transform(visitor))
            .collect();
        self
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

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        self.arguments = self
            .arguments
            .into_iter()
            .map(|a| a.transform(visitor))
            .collect();
        self
    }
}
