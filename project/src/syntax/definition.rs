use super::expression::Expression;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;
use crate::syntax::GlobalVariable;

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
