use super::expression::Expression;
use super::variable::{FreeVariable, GlobalVariable, LocalVariable};
use crate::ast_transform::Transformer;
use crate::source::SourceLocation;
use crate::utils::Sourced;

sum_type! {
    #[derive(Debug, Clone)]
    pub type Reference(Expression) = LocalReference
                                   | GlobalReference
                                   | PredefinedReference
                                   | FreeReference
                                   | IntrinsicReference;
}

impl Reference {
    pub fn default_transform(self, visitor: &mut impl Transformer) -> Self {
        use Reference::*;
        match self {
            LocalReference(x) => x.default_transform(visitor).into(),
            GlobalReference(x) => x.default_transform(visitor).into(),
            PredefinedReference(x) => x.default_transform(visitor).into(),
            FreeReference(x) => x.default_transform(visitor).into(),
            IntrinsicReference(x) => x.default_transform(visitor).into(),
        }
    }
}

impl Sourced for Reference {
    fn source(&self) -> &SourceLocation {
        use Reference::*;
        match self {
            LocalReference(x) => x.source(),
            GlobalReference(x) => x.source(),
            PredefinedReference(x) => x.source(),
            FreeReference(x) => x.source(),
            IntrinsicReference(x) => x.source(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalReference {
    pub var: LocalVariable,
    pub span: SourceLocation,
}

impl_sourced!(LocalReference);

impl PartialEq for LocalReference {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
    }
}

impl LocalReference {
    pub fn new(var: LocalVariable, span: SourceLocation) -> Self {
        LocalReference { var, span }
    }

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}

#[derive(Debug, Clone)]
pub struct GlobalReference {
    pub var: GlobalVariable,
    pub span: SourceLocation,
}

impl_sourced!(GlobalReference);

impl PartialEq for GlobalReference {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
    }
}

impl GlobalReference {
    pub fn new(var: GlobalVariable, span: SourceLocation) -> Self {
        GlobalReference { var, span }
    }

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}

#[derive(Debug, Clone)]
pub struct PredefinedReference {
    pub var: GlobalVariable,
    pub span: SourceLocation,
}

impl_sourced!(PredefinedReference);

impl PartialEq for PredefinedReference {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
    }
}

impl PredefinedReference {
    pub fn new(var: GlobalVariable, span: SourceLocation) -> Self {
        PredefinedReference { var, span }
    }

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}

#[derive(Debug, Clone)]
pub struct IntrinsicReference {
    pub var: GlobalVariable,
    pub span: SourceLocation,
}

impl_sourced!(IntrinsicReference);

impl PartialEq for IntrinsicReference {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
    }
}

impl IntrinsicReference {
    pub fn new(var: GlobalVariable, span: SourceLocation) -> Self {
        IntrinsicReference { var, span }
    }

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}

#[derive(Debug, Clone)]
pub struct FreeReference {
    pub var: FreeVariable,
    span: SourceLocation,
}

impl_sourced!(FreeReference);

impl FreeReference {
    pub fn new(var: FreeVariable, span: SourceLocation) -> Self {
        FreeReference { var, span }
    }

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}
