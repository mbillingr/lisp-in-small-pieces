use super::expression::Expression;
use super::import::Import;
use crate::ast_transform::Transformer;
use crate::scm::{ScmContainer, Scm};
use crate::source::SourceLocation;
use crate::syntax::Reify;

#[derive(Debug, Clone)]
pub struct Program {
    pub imports: Import,
    pub body: Expression,
    span: SourceLocation,
}

impl_sourced!(Program);

impl Program {
    pub fn new(imports: Import, body: Expression, span: SourceLocation) -> Self {
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

impl Reify for Program {
    fn reify(&self) -> Scm {
        let imports = self.imports.reify();
        let body = self.body.reify();
        ScmContainer::vector(vec![imports, body])
    }
}
