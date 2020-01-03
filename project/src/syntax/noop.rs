use crate::ast_transform::Transformer;
use crate::source::SourceLocation;

#[derive(Debug, Clone)]
pub struct NoOp;

impl crate::utils::Sourced for NoOp {
    fn source(&self) -> &SourceLocation {
        &SourceLocation::NoSource
    }
}

impl NoOp {
    pub fn default_transform(mut self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}
