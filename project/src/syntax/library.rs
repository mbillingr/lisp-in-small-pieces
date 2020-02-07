use super::expression::Expression;
use super::Import;
use crate::ast_transform::Transformer;
use crate::env::Env;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::utils::Sourced;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct Library {
    pub env: Env,
    pub imports: Import,
    pub exports: Vec<LibraryExportSpec>,
    pub body: Expression,
    span: SourceLocation,
}

impl_sourced!(Library);

sum_type! {
    #[derive(Debug, Clone)]
    pub type LibraryDeclaration = Expression | Import | LibraryExport;
}

#[derive(Debug, Clone)]
pub struct LibraryExport {
    specs: VecDeque<LibraryExportSpec>,
    span: SourceLocation,
}
impl_sourced!(LibraryExport);

#[derive(Debug, Clone)]
pub enum LibraryExportSpec {
    Identifier(Symbol, SourceLocation),
    Rename(Symbol, Symbol, SourceLocation),
}

impl Sourced for LibraryExportSpec {
    fn source(&self) -> &SourceLocation {
        match self {
            LibraryExportSpec::Identifier(_, span) => span,
            LibraryExportSpec::Rename(_, _, span) => span,
        }
    }
}

impl Library {
    pub fn new(
        env: Env,
        imports: Import,
        exports: LibraryExport,
        body: Expression,
        span: SourceLocation,
    ) -> Self {
        Library {
            env,
            imports,
            exports: exports.specs.into(),
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

impl LibraryExport {
    pub fn new(span: SourceLocation) -> Self {
        LibraryExport {
            specs: VecDeque::new(),
            span,
        }
    }

    pub fn adjoin(&mut self, spec: LibraryExportSpec, span: &SourceLocation) {
        self.span = self.span.start_at(&span);
        self.specs.push_front(spec);
    }

    pub fn adjoin_identifier(&mut self, ident: Symbol, span: &SourceLocation) {
        self.adjoin(LibraryExportSpec::Identifier(ident, span.clone()), span)
    }

    pub fn adjoin_rename(&mut self, old: Symbol, new: Symbol, span: &SourceLocation) {
        self.adjoin(LibraryExportSpec::Rename(old, new, span.clone()), span)
    }

    pub fn extend(&mut self, more: Self) {
        self.specs.extend(more.specs);
    }
}

impl LibraryExportSpec {
    pub fn exported_name(&self) -> Symbol {
        match self {
            LibraryExportSpec::Identifier(s, _) => *s,
            LibraryExportSpec::Rename(_, s, _) => *s,
        }
    }

    pub fn internal_name(&self) -> Symbol {
        match self {
            LibraryExportSpec::Identifier(s, _) => *s,
            LibraryExportSpec::Rename(s, _, _) => *s,
        }
    }
}
