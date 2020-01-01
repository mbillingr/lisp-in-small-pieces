use super::expression::Expression;
use super::reference::LocalReference;
use super::variable::GlobalVariable;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;

sum_types! {
    #[derive(Debug, Clone)]
    pub type Assignment = LocalAssignment | GlobalAssignment;
}

impl Assignment {
    pub fn default_transform(self, visitor: &mut impl Transformer) -> Self {
        use Assignment::*;
        match self {
            LocalAssignment(x) => x.default_transform(visitor).into(),
            GlobalAssignment(x) => x.default_transform(visitor).into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalAssignment {
    pub reference: LocalReference,
    pub form: Box<Expression>,
    span: SourceLocation,
}

impl LocalAssignment {
    pub fn new(
        reference: LocalReference,
        form: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        LocalAssignment {
            reference,
            form: form.into(),
            span,
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        *self.form = self.form.transform(visitor);
        self
    }
}

#[derive(Debug, Clone)]
pub struct GlobalAssignment {
    pub variable: GlobalVariable,
    pub form: Box<Expression>,
    span: SourceLocation,
}

impl GlobalAssignment {
    pub fn new(
        variable: GlobalVariable,
        form: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        GlobalAssignment {
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
