use super::expression::Expression;
use super::import::Import;
use crate::ast_transform::{Transformer, Visited};
use crate::source::SourceLocation;

#[derive(Debug, Clone)]
pub struct Program {
    pub imports: Vec<Import>,
    pub body: Expression,
    span: SourceLocation,
}

impl_sourced!(Program);

impl Program {
    pub fn new(imports: Vec<Import>, body: Expression, span: SourceLocation) -> Self {
        Program {
            imports,
            body,
            span,
        }
    }

    pub fn transform(mut self, visitor: &mut impl Transformer) -> Self {
        //self.imports = self.imports.into_iter().map(|import| import.transform(visitor)).collect();
        self.body = self.body.transform(visitor);
        self
    }
}
