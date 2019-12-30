use super::variable::{GlobalVariable, LocalVariable, PredefinedVariable};
use crate::source::SourceLocation;

// TODO: are different reference types required, or could we use simply one type and
//       distinguish by the referenced variable's type?

sum_types! {
    #[derive(Debug, Clone)]
    pub type Reference = LocalReference | GlobalReference | PredefinedReference;
}

#[derive(Debug, Clone)]
pub struct LocalReference {
    pub var: LocalVariable,
    pub span: SourceLocation,
}

impl PartialEq for LocalReference {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
    }
}

impl LocalReference {
    pub fn new(var: LocalVariable, span: SourceLocation) -> Self {
        LocalReference { var, span }
    }
}

#[derive(Debug, Clone)]
pub struct GlobalReference {
    pub var: GlobalVariable,
    pub span: SourceLocation,
}

impl PartialEq for GlobalReference {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
    }
}

impl GlobalReference {
    pub fn new(var: GlobalVariable, span: SourceLocation) -> Self {
        GlobalReference { var, span }
    }
}

#[derive(Debug, Clone)]
pub struct PredefinedReference {
    pub var: PredefinedVariable,
    pub span: SourceLocation,
}

impl PartialEq for PredefinedReference {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
    }
}

impl PredefinedReference {
    pub fn new(var: PredefinedVariable, span: SourceLocation) -> Self {
        PredefinedReference { var, span }
    }
}
