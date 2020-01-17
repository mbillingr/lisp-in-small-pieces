use crate::ast_transform::Transformer;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::utils::Sourced;
use std::collections::HashMap;
use std::path::PathBuf;

sum_types! {
    #[derive(Debug, Clone)]
    pub type Import = ImportAll
                    | ImportOnly
                    | ImportExcept
                    | ImportPrefix
                    | ImportRename;
}

impl Import {
    pub fn default_transform(self, visitor: &mut impl Transformer) -> Self {
        use Import::*;
        match self {
            ImportAll(x) => x.default_transform(visitor).into(),
            ImportOnly(x) => x.default_transform(visitor).into(),
            ImportExcept(x) => x.default_transform(visitor).into(),
            ImportPrefix(x) => x.default_transform(visitor).into(),
            ImportRename(x) => x.default_transform(visitor).into(),
        }
    }
}

impl Sourced for Import {
    fn source(&self) -> &SourceLocation {
        use Import::*;
        match self {
            ImportAll(x) => x.source(),
            ImportOnly(x) => x.source(),
            ImportExcept(x) => x.source(),
            ImportPrefix(x) => x.source(),
            ImportRename(x) => x.source(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportAll {
    pub library_name: PathBuf,
    pub span: SourceLocation,
}
impl_sourced!(ImportAll);

impl ImportAll {
    pub fn new(library_name: impl Into<PathBuf>, span: SourceLocation) -> Self {
        ImportAll {
            library_name: library_name.into(),
            span,
        }
    }
    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}

#[derive(Debug, Clone)]
pub struct ImportOnly {
    library_name: PathBuf,
    identifiers: Vec<Symbol>,
    pub span: SourceLocation,
}
impl_sourced!(ImportOnly);

impl ImportOnly {
    pub fn new(
        library_name: impl Into<PathBuf>,
        identifiers: Vec<Symbol>,
        span: SourceLocation,
    ) -> Self {
        ImportOnly {
            library_name: library_name.into(),
            identifiers,
            span,
        }
    }
    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}

#[derive(Debug, Clone)]
pub struct ImportExcept {
    library_name: PathBuf,
    identifiers: Vec<Symbol>,
    pub span: SourceLocation,
}
impl_sourced!(ImportExcept);

impl ImportExcept {
    pub fn new(
        library_name: impl Into<PathBuf>,
        identifiers: Vec<Symbol>,
        span: SourceLocation,
    ) -> Self {
        ImportExcept {
            library_name: library_name.into(),
            identifiers,
            span,
        }
    }
    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}

#[derive(Debug, Clone)]
pub struct ImportPrefix {
    library_name: PathBuf,
    prefix: Symbol,
    pub span: SourceLocation,
}
impl_sourced!(ImportPrefix);

impl ImportPrefix {
    pub fn new(library_name: impl Into<PathBuf>, prefix: Symbol, span: SourceLocation) -> Self {
        ImportPrefix {
            library_name: library_name.into(),
            prefix,
            span,
        }
    }
    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}

#[derive(Debug, Clone)]
pub struct ImportRename {
    pub library_name: PathBuf,
    pub mapping: HashMap<Symbol, Symbol>,
    pub span: SourceLocation,
}
impl_sourced!(ImportRename);

impl ImportRename {
    pub fn new(
        library_name: impl Into<PathBuf>,
        mapping: impl Into<HashMap<Symbol, Symbol>>,
        span: SourceLocation,
    ) -> Self {
        ImportRename {
            library_name: library_name.into(),
            mapping: mapping.into(),
            span,
        }
    }
    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }
}
