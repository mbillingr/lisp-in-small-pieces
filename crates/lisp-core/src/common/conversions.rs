// Unfortunately, we can't do this:
//    impl<T: BasicLispValue> From<()> for T {
//        fn from(_: ()) -> T { T::N }
//    }
//
// So at least we provide a macro to lift
// some implementation burden from the user.

/// Implement some common conversions from Rust types into Lisp values
#[macro_export]
macro_rules! impl_conversions {
    ($T:ty) => {
        impl From<()> for $T {
            fn from(_: ()) -> $T {
                <$T>::nil()
            }
        }

        impl<A: Into<$T>, B: Into<$T>> From<(A, B)> for $T {
            fn from((a, b): (A, B)) -> X {
                <$T>::cons(a.into(), b.into())
            }
        }
    };
}
