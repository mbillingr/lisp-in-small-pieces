use super::expression::Expression;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;
use crate::syntax::Reify;
use crate::scm::Scm;

#[derive(Debug, Clone)]
pub struct Application {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
    span: SourceLocation,
}

impl_sourced!(Application);

impl Application {
    pub fn new(
        function: impl Into<Box<Expression>>,
        arguments: Vec<Expression>,
        span: SourceLocation,
    ) -> Self {
        Application {
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

impl Reify for Application {
    fn reify(&self) -> Scm {
        let args = Scm::list(self.arguments.iter().map(Reify::reify));
        Scm::cons(self.function.reify(), args)
    }
}
