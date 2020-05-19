use crate::ast_transform::Transformer;
use sunny_parser::{SourceLocation, Sourced};

#[derive(Debug, Clone)]
pub struct NoOp;

impl Sourced for NoOp {
    fn source(&self) -> &SourceLocation {
        &SourceLocation::NoSource
    }
}

impl NoOp {
    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}
