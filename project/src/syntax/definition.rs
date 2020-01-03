use super::expression::Expression;
use super::variable::Variable;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::syntax::GlobalVariable;
use crate::utils::Sourced;

#[derive(Debug, Clone)]
pub struct Definition {
    pub var_name: Symbol,
    pub form: Box<Expression>,
    pub span: SourceLocation,
}

impl_sourced!(Definition);

#[derive(Debug, Clone)]
pub struct GlobalDefine {
    pub variable: GlobalVariable,
    pub form: Box<Expression>,
    span: SourceLocation,
}

impl_sourced!(GlobalDefine);

impl Definition {
    pub fn new(var_name: Symbol, form: impl Into<Box<Expression>>, span: SourceLocation) -> Self {
        Definition {
            var_name,
            form: form.into(),
            span,
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        *self.form = self.form.transform(visitor);
        self
    }
}

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
