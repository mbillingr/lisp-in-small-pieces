use super::expression::Expression;
use super::variable::LocalVariable;
use crate::ast_transform::Transformer;
use crate::scm::Scm;
use crate::syntax::Reify;
use sunny_common::Named;
use sunny_parser::{impl_sourced, SourceLocation};

#[derive(Debug, Clone)]
pub struct FixLet {
    pub variables: Vec<LocalVariable>,
    pub arguments: Vec<Expression>,
    pub body: Box<Expression>,
    pub span: SourceLocation,
}

impl_sourced!(FixLet);

impl FixLet {
    pub fn new(
        variables: Vec<LocalVariable>,
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

impl Reify for FixLet {
    fn reify(&self) -> Scm {
        let init = self
            .variables
            .iter()
            .zip(&self.arguments)
            .rev()
            .map(|(v, a)| Scm::list(vec![Scm::Symbol(v.name()), a.reify()]));
        let body = self.body.reify();
        Scm::list(vec![Scm::symbol("let"), Scm::list(init), body])
    }
}
