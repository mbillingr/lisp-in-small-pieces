use crate::ast_transform::Transformer;
use crate::library::ExportItem;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::utils::Sourced;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct Import {
    pub import_sets: Vec<ImportSet>,
    pub span: SourceLocation,
}
impl_sourced!(Import);

impl Import {
    pub fn new(import_sets: Vec<ImportSet>, span: SourceLocation) -> Self {
        Import { import_sets, span }
    }

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }

    pub fn join(mut self, other: Self) -> Self {
        self.import_sets.extend(other.import_sets);
        self.span = self.span.join(&other.span);
        self
    }
}

#[derive(Debug, Clone)]
pub struct ImportSet {
    pub library_name: PathBuf,
    pub items: HashSet<ImportItem>,
    pub span: SourceLocation,
}
impl_sourced!(ImportSet);

impl ImportSet {
    pub fn new(library_name: PathBuf, items: HashSet<ImportItem>, span: SourceLocation) -> Self {
        ImportSet {
            library_name,
            items: items.into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportItem {
    pub export_name: Symbol,
    pub import_name: Symbol,
    pub item: ExportItem,
}

impl ImportItem {
    pub fn from_export((export_name, item_ref): (Symbol, &ExportItem)) -> Self {
        ImportItem {
            export_name,
            import_name: export_name,
            item: item_ref.clone(),
        }
    }
}

impl PartialEq for ImportItem {
    fn eq(&self, other: &Self) -> bool {
        self.export_name == other.export_name && self.import_name == other.import_name
    }
}

impl Eq for ImportItem {}

impl Hash for ImportItem {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.export_name.hash(state);
        self.import_name.hash(state);
    }
}

/*

sum_types! {
    #[derive(Debug, Clone)]
    pub type ImportSet = ImportAll
                       | ImportOnly
                       | ImportExcept
                       | ImportPrefix
                       | ImportRename;
}

impl ImportSet {
    pub fn default_transform(self, visitor: &mut impl Transformer) -> Self {
        use ImportSet::*;
        match self {
            ImportAll(x) => x.default_transform(visitor).into(),
            ImportOnly(x) => x.default_transform(visitor).into(),
            ImportExcept(x) => x.default_transform(visitor).into(),
            ImportPrefix(x) => x.default_transform(visitor).into(),
            ImportRename(x) => x.default_transform(visitor).into(),
        }
    }

    pub fn identifiers(&self) -> impl Iterator<Item=(Symbol, &ExportItem)> {
        use ImportSet::*;
        match self {
            ImportAll(x) => x.identifiers(),
            ImportOnly(x) => x.identifiers(),
            ImportExcept(x) => x.identifiers(),
            ImportPrefix(x) => x.identifiers(),
            ImportRename(x) => x.identifiers(),
        }
    }
}

impl Sourced for ImportSet {
    fn source(&self) -> &SourceLocation {
        use ImportSet::*;
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
    import_set: Box<ImportSet>,
    identifiers: Vec<Symbol>,
    pub span: SourceLocation,
}
impl_sourced!(ImportOnly);

impl ImportOnly {
    pub fn new(
        import_set: ImportSet,
        identifiers: Vec<Symbol>,
        span: SourceLocation,
    ) -> Self {
        ImportOnly {
            import_set: Box::new(import_set),
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
    import_set: Box<ImportSet>,
    identifiers: Vec<Symbol>,
    pub span: SourceLocation,
}
impl_sourced!(ImportExcept);

impl ImportExcept {
    pub fn new(
        import_set: ImportSet,
        identifiers: Vec<Symbol>,
        span: SourceLocation,
    ) -> Self {
        ImportExcept {
            import_set: Box::new(import_set),
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
    import_set: Box<ImportSet>,
    prefix: Symbol,
    pub span: SourceLocation,
}
impl_sourced!(ImportPrefix);

impl ImportPrefix {
    pub fn new(
        import_set: ImportSet, prefix: Symbol, span: SourceLocation) -> Self {
        ImportPrefix {
            import_set: Box::new(import_set),
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
    import_set: Box<ImportSet>,
    pub mapping: HashMap<Symbol, Symbol>,
    pub span: SourceLocation,
}
impl_sourced!(ImportRename);

impl ImportRename {
    pub fn new(
        import_set: ImportSet,
        mapping: impl Into<HashMap<Symbol, Symbol>>,
        span: SourceLocation,
    ) -> Self {
        ImportRename {
            import_set: Box::new(import_set),
            mapping: mapping.into(),
            span,
        }
    }

    pub fn default_transform(self, _visitor: &mut impl Transformer) -> Self {
        self
    }

    pub fn identifiers(&self) -> impl Iterator<Item=(Symbol, &ExportItem)> {
        self.import_set
            .identifiers()
            .map(|(s, item)| {
                (*self.mapping.get(&s).unwrap_or(&s), item)
            })
    }
}
*/
