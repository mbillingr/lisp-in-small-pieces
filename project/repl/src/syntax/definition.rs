use super::expression::Expression;
use crate::ast_transform::Transformer;
use crate::syntax::GlobalVariable;
use sunny_common::{impl_sourced, SourceLocation};

#[derive(Debug, Clone)]
pub struct GlobalDefine {
    pub variable: GlobalVariable,
    pub form: Box<Expression>,
    pub redefine: bool,
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
            redefine: false,
            span,
        }
    }
    pub fn redefine(
        variable: GlobalVariable,
        form: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        GlobalDefine {
            variable,
            form: form.into(),
            redefine: true,
            span,
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        *self.form = self.form.transform(visitor);
        self
    }
}
