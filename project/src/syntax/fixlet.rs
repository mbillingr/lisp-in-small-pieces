use super::expression::Expression;
use super::variable::LocalVariable;
use crate::ast_transform::Transformer;
use crate::scm::{ScmContainer, Scm};
use crate::source::SourceLocation;
use crate::syntax::Reify;
use crate::utils::Named;

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
            .map(|(v, a)| ScmContainer::list(vec![ScmContainer::symbol(v.name()), a.reify()]));
        let body = self.body.reify();
        ScmContainer::list(vec![ScmContainer::symbol("let"), ScmContainer::list(init), body])
    }
}
