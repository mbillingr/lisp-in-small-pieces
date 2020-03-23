use crate::source::SourceLocation;
use std::path::{Path, PathBuf};

pub trait Named {
    type Name: PartialEq;
    fn name(&self) -> Self::Name;
}

pub trait Sourced {
    fn source(&self) -> &SourceLocation;
}

macro_rules! impl_sourced {
    ($t:ty) => {
        impl_sourced!($t: span);
    };

    ($t:ty: $field:ident) => {
        impl_sourced!($t: self.$field);
    };

    ($t:ty: self.$($tokens:tt)*) => {
        impl crate::utils::Sourced for $t {
            fn source(&self) -> &crate::source::SourceLocation {
                &self.$($tokens)*
            }
        }
    };
}

macro_rules! splice {
    ($a:expr, $($rest:tt)*) => {{
        let mut collection = $a;
        collection.extend(splice!(@chain $($rest)*));
        collection
    }};

    (@chain $b:expr) => {$b.into_iter()};

    (@chain $b:expr, $($rest:tt)*) => {
        $b.into_iter().chain(splice!(@chain $($rest)*))
    };
}

pub fn find_path(path: &Path, prefixes: &[PathBuf]) -> Option<PathBuf> {
    for prefix in prefixes {
        let p = prefix.join(path);
        if p.exists() {
            return Some(p);
        }
    }
    None
}

pub fn find_library(library_path: &Path) -> Option<PathBuf> {
    let file_path = library_path.with_extension("sld");
    find_path(
        &file_path,
        &[
            PathBuf::from("."),
            PathBuf::from("../libs"),
            PathBuf::from("libs"),
            dirs::data_dir().unwrap().join("scheme-libs"),
        ],
    )
}
