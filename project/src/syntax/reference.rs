use super::expression::Expression;
use super::variable::{FreeVariable, GlobalVariable, LocalVariable};
use crate::ast_transform::{Transformer, Visited};
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::syntax::Variable;
use crate::utils::{Named, Sourced};

sum_type! {
    #[derive(Debug, Clone, PartialEq)]
    pub type Reference(Expression) = LocalReference
                                   | GlobalReference
                                   | PredefinedReference
                                   | FreeReference;
}

impl Reference {
    pub fn default_transform(self, visitor: &mut impl Transformer) -> Self {
        use Reference::*;
        match self {
            LocalReference(x) => x.default_transform(visitor).into(),
            GlobalReference(x) => x.default_transform(visitor).into(),
            PredefinedReference(x) => x.default_transform(visitor).into(),
            FreeReference(x) => x.default_transform(visitor).into(),
        }
    }

    pub fn var_name(&self) -> Symbol {
        use Reference::*;
        match self {
            LocalReference(r) => r.var.name(),
            GlobalReference(r) => r.var.name(),
            PredefinedReference(r) => r.var.name(),
            FreeReference(r) => r.var.name(),
        }
    }

    pub fn var(&self) -> Variable {
        use Reference::*;
        match self {
            LocalReference(r) => r.var.clone().into(),
            GlobalReference(r) => r.var.clone().into(),
            PredefinedReference(r) => r.var.clone().into(),
            FreeReference(r) => r.var.clone().into(),
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
pub struct FreeReference {
    pub var: FreeVariable,
    span: SourceLocation,
}

impl_sourced!(FreeReference);

impl PartialEq for FreeReference {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
    }
}

impl FreeReference {
    pub fn new(var: FreeVariable, span: SourceLocation) -> Self {
        FreeReference { var, span }
    }

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}
