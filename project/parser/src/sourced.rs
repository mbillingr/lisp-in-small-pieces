use crate::SourceLocation;

pub trait Sourced {
    fn source(&self) -> &SourceLocation;
}

#[macro_export]
macro_rules! impl_sourced {
    ($t:ty) => {
        impl_sourced!($t: span);
    };

    ($t:ty: $field:ident) => {
        impl_sourced!($t: self.$field);
    };

    ($t:ty: self.$($tokens:tt)*) => {
        impl sunny_parser::Sourced for $t {
            fn source(&self) -> &sunny_parser::SourceLocation {
                &self.$($tokens)*
            }
        }
    };
}
