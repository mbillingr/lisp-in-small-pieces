use super::expression::Expression;
use super::variable::LocalVariable;
use crate::ast_transform::Transformer;
use crate::primitive::Arity;
use crate::scm::Scm;
use crate::syntax::Reify;
use std::convert::TryInto;
use sunny_common::{impl_sourced, Named, SourceLocation};

#[derive(Debug, Clone)]
pub struct Function {
    pub variables: Vec<LocalVariable>,
    pub body: Box<Expression>,
    pub span: SourceLocation,
}

impl_sourced!(Function);

impl Function {
    pub fn new(
        variables: Vec<LocalVariable>,
        body: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        Function {
            variables,
            body: body.into(),
            span,
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        *self.body = self.body.transform(visitor);
        self
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

impl Reify for Function {
    fn reify(&self) -> Scm {
        let mut vars = Scm::nil();
        for v in self.variables.iter().rev() {
            let name = Scm::Symbol(v.name());
            if v.is_dotted() {
                vars = name;
            } else {
                vars = Scm::cons(name, vars);
            }
        }
        let body = self.body.reify();
        Scm::list(vec![Scm::symbol("lambda"), vars, body])
    }
}
