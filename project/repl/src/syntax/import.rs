use crate::ast_transform::Transformer;
use crate::scm::Scm;
use crate::syntax::{Reify, Variable};
use crate::utils::Named;
use std::iter::FromIterator;
use std::path::PathBuf;
use sunny_common::Symbol;
use sunny_parser::SourceLocation;
use sunny_parser::SourceLocation::NoSource;

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

    pub fn empty() -> Self {
        Import {
            import_sets: vec![],
            span: NoSource,
        }
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
    pub library_symb: Symbol,
    pub items: Vec<ImportItem>,
    pub span: SourceLocation,
}
impl_sourced!(ImportSet);

impl ImportSet {
    pub fn new(
        library_name: Scm,
        library_path: PathBuf,
        items: Vec<ImportItem>,
        span: SourceLocation,
    ) -> Self {
        ImportSet {
            library_symb: library_path.to_str().unwrap().into(),
            library_path,
            library_name,
            items: items.into(),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportItem {
    pub export_var: Variable,
    pub import_var: Variable,
}

impl ImportItem {
    pub fn new(export_var: Variable, import_var: Variable) -> Self {
        ImportItem {
            export_var,
            import_var,
        }
    }
}

impl PartialEq for ImportItem {
    fn eq(&self, other: &Self) -> bool {
        self.export_var == other.export_var && self.import_var == other.import_var
    }
}

impl Eq for ImportItem {}

/*impl Hash for ImportItem {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.export_var.hash(state);
        self.import_var.hash(state);
    }
}*/

impl Reify for Import {
    fn reify(&self) -> Scm {
        let imports = self.import_sets.iter().map(Reify::reify);
        Scm::vector(imports)
    }
}

impl Reify for ImportSet {
    fn reify(&self) -> Scm {
        let items = self.items.iter().map(|item| {
            if item.export_var == item.import_var {
                Scm::Symbol(item.export_var.name())
            } else {
                Scm::list(vec![
                    Scm::Symbol(item.export_var.name()),
                    Scm::Symbol(item.import_var.name()),
                ])
            }
        });
        Scm::list(vec![
            Scm::symbol("import"),
            self.library_name,
            Scm::list(items),
        ])
    }
}
