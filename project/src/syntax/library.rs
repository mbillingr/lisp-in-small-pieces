use super::expression::Expression;
use super::Import;
use crate::ast_transform::Transformer;
use crate::env::Env;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct Library {
    pub env: Env,
    pub imports: Vec<Import>,
    pub exports: Vec<LibraryExportSpec>,
    pub body: Expression,
    span: SourceLocation,
}

impl_sourced!(Library);

sum_type! {
    #[derive(Debug, Clone)]
    pub type LibraryDeclaration = Expression | LibraryImport | LibraryExport;
}

#[derive(Debug, Clone)]
pub struct LibraryImport {}

#[derive(Debug, Clone)]
pub struct LibraryExport {
    specs: VecDeque<LibraryExportSpec>,
}

#[derive(Debug, Copy, Clone)]
pub enum LibraryExportSpec {
    Identifier(Symbol),
    Rename(Symbol, Symbol),
}

impl Library {
    pub fn new(
        env: Env,
        imports: LibraryImport,
        exports: LibraryExport,
        body: Expression,
        span: SourceLocation,
    ) -> Self {
        Library {
            env,
            imports: vec![],
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
    pub fn new() -> Self {
        LibraryExport {
            specs: VecDeque::new(),
        }
    }

    pub fn adjoin(&mut self, spec: LibraryExportSpec) {
        self.specs.push_front(spec);
    }

    pub fn adjoin_identifier(&mut self, ident: Symbol) {
        self.adjoin(LibraryExportSpec::Identifier(ident))
    }

    pub fn adjoin_rename(&mut self, old: Symbol, new: Symbol) {
        self.adjoin(LibraryExportSpec::Rename(old, new))
    }

    pub fn extend(&mut self, more: Self) {
        self.specs.extend(more.specs);
    }
}

impl LibraryImport {
    pub fn new() -> Self {
        LibraryImport {}
    }
}

impl LibraryExportSpec {
    pub fn exported_name(&self) -> Symbol {
        match self {
            LibraryExportSpec::Identifier(s) => *s,
            LibraryExportSpec::Rename(_, s) => *s,
        }
    }

    pub fn internal_name(&self) -> Symbol {
        match self {
            LibraryExportSpec::Identifier(s) => *s,
            LibraryExportSpec::Rename(s, _) => *s,
        }
    }
}
