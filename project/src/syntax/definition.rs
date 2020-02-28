use super::expression::Expression;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;
use crate::syntax::{GlobalVariable, Reify};
use crate::scm::Scm;
use crate::utils::Named;

#[derive(Debug, Clone)]
pub struct GlobalDefine {
    pub variable: GlobalVariable,
    pub form: Box<Expression>,
    span: SourceLocation,
}

impl_sourced!(GlobalDefine);

impl GlobalDefine {
    pub fn new(
        variable: GlobalVariable,
        form: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        GlobalDefine {
            variable,
            form: form.into(),
            span,
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        *self.form = self.form.transform(visitor);
        self
    }
}

impl Reify for GlobalDefine {
    fn reify(&self) -> Scm {
        Scm::list(vec![Scm::symbol("define$"), Scm::Symbol(self.variable.name()), self.form.reify()])
    }
}