use crate::ast_transform::Transformer;
use crate::library::ExportItem;
use crate::scm::Scm;
use crate::source::SourceLocation;
use crate::source::SourceLocation::NoSource;
use crate::symbol::Symbol;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
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

impl FromIterator<Import> for Import {
    fn from_iter<T: IntoIterator<Item = Import>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        if let Some(first) = iter.next() {
            let mut imports = first;
            for im in iter {
                imports = imports.join(im);
            }
            imports
        } else {
            Import::new(vec![], NoSource)
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportSet {
    pub library_name: Scm,
    pub library_path: PathBuf,
    pub items: HashSet<ImportItem>,
    pub span: SourceLocation,
}
impl_sourced!(ImportSet);

impl ImportSet {
    pub fn new(
        library_name: Scm,
        library_path: PathBuf,
        items: HashSet<ImportItem>,
        span: SourceLocation,
    ) -> Self {
        ImportSet {
            library_path,
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
