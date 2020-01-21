use super::expression::Expression;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;
use crate::syntax::GlobalVariable;
use crate::utils::Sourced;

sum_type! {
    #[derive(Debug, Clone)]
    pub type Application(Expression) = RegularApplication | PredefinedApplication;
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

impl Sourced for Application {
    fn source(&self) -> &SourceLocation {
        use Application::*;
        match self {
            RegularApplication(x) => x.source(),
            PredefinedApplication(x) => x.source(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RegularApplication {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    span: SourceLocation,
}

impl_sourced!(RegularApplication);

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
    pub variable: GlobalVariable,
    pub arguments: Vec<Expression>,
    span: SourceLocation,
}

impl_sourced!(PredefinedApplication);

impl PredefinedApplication {
    pub fn new(variable: GlobalVariable, arguments: Vec<Expression>, span: SourceLocation) -> Self {
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
