use super::expression::Expression;
use super::reference::LocalReference;
use super::variable::LocalVariable;
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;
use crate::syntax::Reference;
use std::convert::TryInto;

#[derive(Debug, Clone)]
pub struct BoxRead {
    pub reference: Reference,
    span: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct BoxWrite {
    pub reference: Reference,
    pub form: Box<Expression>,
    span: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct BoxCreate {
    pub variable: LocalVariable,
    span: SourceLocation,
}

impl_sourced!(BoxRead);
impl_sourced!(BoxWrite);
impl_sourced!(BoxCreate);

impl BoxRead {
    pub fn new(reference: Reference, span: SourceLocation) -> Self {
        BoxRead { reference, span }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        self.reference = Expression::from(self.reference)
            .transform(visitor)
            .try_into()
            .unwrap_or_else(|x| panic!("Expected `LocalReference` but got {:?}", x));
        self
    }
}

impl BoxWrite {
    pub fn new(
        reference: Reference,
        form: impl Into<Box<Expression>>,
        span: SourceLocation,
    ) -> Self {
        BoxWrite {
            reference,
            form: form.into(),
            span,
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        self.reference = Expression::from(self.reference)
            .transform(visitor)
            .try_into()
            .unwrap_or_else(|x| panic!("Expected `LocalReference` but got {:?}", x));
        *self.form = self.form.transform(visitor);
        self
    }
}

impl BoxCreate {
    pub fn new(variable: LocalVariable, span: SourceLocation) -> Self {
        BoxCreate { variable, span }
    }

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}
