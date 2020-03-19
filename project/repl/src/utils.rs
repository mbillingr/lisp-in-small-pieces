use crate::source::SourceLocation;

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
