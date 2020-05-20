use super::expression::Expression;
use super::reference::LocalReference;
use super::variable::GlobalVariable;
use crate::ast_transform::Transformer;
use crate::scm::Scm;
use crate::syntax::Reify;
use sunny_common::{impl_sourced, sum_type, Named, SourceLocation, Sourced};

sum_type! {
    #[derive(Debug, Clone)]
    pub type Assignment(Expression) = LocalAssignment | GlobalAssignment;
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

impl Sourced for Assignment {
    fn source(&self) -> &SourceLocation {
        use Assignment::*;
        match self {
            LocalAssignment(x) => &x.span,
            GlobalAssignment(x) => &x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalAssignment {
    pub reference: LocalReference,
    pub form: Box<Expression>,
    pub span: SourceLocation,
}

impl_sourced!(LocalAssignment);

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

impl_sourced!(GlobalAssignment);

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

impl Reify for Assignment {
    fn reify(&self) -> Scm {
        let var = match self {
            Assignment::LocalAssignment(a) => Scm::Symbol(a.reference.var.name()),
            Assignment::GlobalAssignment(a) => Scm::Symbol(a.variable.name()),
        };

        let val = match self {
            Assignment::LocalAssignment(a) => a.form.reify(),
            Assignment::GlobalAssignment(a) => a.form.reify(),
        };

        Scm::list(vec![Scm::symbol("set!"), var, val])
    }
}
