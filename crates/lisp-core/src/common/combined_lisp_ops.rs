use super::{BasicLispValue, Result};

/// Macro to define cons lists; automatically converts items using `Into<_>`
#[macro_export]
macro_rules! list {
    (@) => { () };
    (@ $x:expr) => { ($x, ()) };
    (@ $x:expr, $($rest:expr),*) => { ($x, list!(@ $($rest),*)) };

    ($($args:tt)*) => { list!(@ $($args)*).into() };
}

/// Macro for defining compound car/cdr access chains.
/// `cxr!(expr => c a d d r)` expands to something like
/// `expr.cdr()?.cdr()?.car()?`, which is equivalent to
/// `(car (cdr (cdr expr)))` or `(caddr expr)` in Lisp.
#[macro_export]
macro_rules! cxr {
    ($obj:expr => c r) => {
        $obj
    };

    ($obj:expr => c a $($rest:ident)*) => {
        cxr!($obj => c $($rest)*).car()?
    };

    ($obj:expr => c d $($rest:ident)*) => {
        cxr!($obj => c $($rest)*).cdr()?
    };
}

impl<T: BasicLispValue> CombinedLispOps for T {}

/// All kinds of useful operations on Lisp values.
/// Provides default implementation using only methods from BasicLispValue
/// and generic conversions.
pub trait CombinedLispOps: BasicLispValue {
    fn list<T: Into<Self>, I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut iter = iter.into_iter();
        match iter.next() {
            Some(first) => Self::cons(first.into(), Self::list(iter)),
            None => Self::nil(),
        }
    }

    fn caar(&self) -> Result<&Self> {
        Ok(cxr!(self => c a a r))
    }
    fn cadr(&self) -> Result<&Self> {
        Ok(cxr!(self => c a d r))
    }
    fn cdar(&self) -> Result<&Self> {
        Ok(cxr!(self => c d a r))
    }
    fn cddr(&self) -> Result<&Self> {
        Ok(cxr!(self => c d d r))
    }

    fn caaar(&self) -> Result<&Self> {
        Ok(cxr!(self => c a a a r))
    }
    fn caadr(&self) -> Result<&Self> {
        Ok(cxr!(self => c a a d r))
    }
    fn cadar(&self) -> Result<&Self> {
        Ok(cxr!(self => c a d a r))
    }
    fn caddr(&self) -> Result<&Self> {
        Ok(cxr!(self => c a d d r))
    }
    fn cdaar(&self) -> Result<&Self> {
        Ok(cxr!(self => c d a a r))
    }
    fn cdadr(&self) -> Result<&Self> {
        Ok(cxr!(self => c d a d r))
    }
    fn cddar(&self) -> Result<&Self> {
        Ok(cxr!(self => c d d a r))
    }
    fn cdddr(&self) -> Result<&Self> {
        Ok(cxr!(self => c d d d r))
    }

    fn caaaar(&self) -> Result<&Self> {
        Ok(cxr!(self => c a a a a r))
    }
    fn caaadr(&self) -> Result<&Self> {
        Ok(cxr!(self => c a a a d r))
    }
    fn caadar(&self) -> Result<&Self> {
        Ok(cxr!(self => c a a d a r))
    }
    fn caaddr(&self) -> Result<&Self> {
        Ok(cxr!(self => c a a d d r))
    }
    fn cadaar(&self) -> Result<&Self> {
        Ok(cxr!(self => c a d a a r))
    }
    fn cadadr(&self) -> Result<&Self> {
        Ok(cxr!(self => c a d a d r))
    }
    fn caddar(&self) -> Result<&Self> {
        Ok(cxr!(self => c a d d a r))
    }
    fn cadddr(&self) -> Result<&Self> {
        Ok(cxr!(self => c a d d d r))
    }
    fn cdaaar(&self) -> Result<&Self> {
        Ok(cxr!(self => c d a a a r))
    }
    fn cdaadr(&self) -> Result<&Self> {
        Ok(cxr!(self => c d a a d r))
    }
    fn cdadar(&self) -> Result<&Self> {
        Ok(cxr!(self => c d a d a r))
    }
    fn cdaddr(&self) -> Result<&Self> {
        Ok(cxr!(self => c d a d d r))
    }
    fn cddaar(&self) -> Result<&Self> {
        Ok(cxr!(self => c d d a a r))
    }
    fn cddadr(&self) -> Result<&Self> {
        Ok(cxr!(self => c d d a d r))
    }
    fn cdddar(&self) -> Result<&Self> {
        Ok(cxr!(self => c d d d a r))
    }
    fn cddddr(&self) -> Result<&Self> {
        Ok(cxr!(self => c d d d d r))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cxr() {
        let x = X::cons(X::cons(X::I(1), X::I(2)), X::cons(X::I(3), X::I(4)));
        (|| -> Result<()> {
            assert_eq!(cxr!(x => c a a r), &1);
            assert_eq!(cxr!(x => c d a r), &2);
            assert_eq!(cxr!(x => c a d r), &3);
            assert_eq!(cxr!(x => c d d r), &4);
            Ok(())
        })()
        .unwrap();
    }

    #[test]
    fn macro_list() {
        let empty: X = list![];
        let one: X = list![1];
        let two: X = list![1, 2];
        let three: X = list![1, 2, 3];
        assert_eq!(empty, X::N);
        assert_eq!(one, X::P(Box::new(X::I(1)), Box::new(X::N)));
        assert_eq!(
            two,
            X::P(
                Box::new(X::I(1)),
                Box::new(X::P(Box::new(X::I(2)), Box::new(X::N)))
            )
        );
        assert_eq!(
            three,
            X::P(
                Box::new(X::I(1)),
                Box::new(X::P(
                    Box::new(X::I(2)),
                    Box::new(X::P(Box::new(X::I(3)), Box::new(X::N)))
                ))
            )
        );
    }

    #[test]
    fn dynamic_list() {
        let list = X::list(vec![1, 2, 3, 4, 5]);
        let expected: X = list![1, 2, 3, 4, 5];
        assert_eq!(list, expected)
    }

    #[derive(Debug)]
    enum X {
        N,
        I(i64),
        P(Box<X>, Box<X>),
    }

    impl_conversions! {X}

    impl BasicLispValue for X {
        type Symbol = ();
        type Procedure = ();

        fn nil() -> Self {
            X::N
        }
        fn bool(_: bool) -> Self {
            unimplemented!()
        }
        fn char(_: char) -> Self {
            unimplemented!()
        }
        fn symbol(_: <Self as BasicLispValue>::Symbol) -> Self {
            unimplemented!()
        }
        fn cons<A: Into<Self>, D: Into<Self>>(car: A, cdr: D) -> Self {
            X::P(Box::new(car.into()), Box::new(cdr.into()))
        }
        fn as_bool(&self) -> Option<bool> {
            unimplemented!()
        }
        fn as_symbol(&self) -> Option<&<Self as BasicLispValue>::Symbol> {
            unimplemented!()
        }
        fn as_pair_mut(&self) -> Option<(&mut Self, &mut Self)> {
            unimplemented!()
        }
        fn as_procedure(&self) -> Option<&Self::Procedure> {
            unimplemented!()
        }
        fn is_null(&self) -> bool {
            unimplemented!()
        }

        fn is_number(&self) -> bool {
            match self {
                X::I(_) => true,
                _ => false,
            }
        }

        fn as_pair(&self) -> Option<(&Self, &Self)> {
            match self {
                X::P(car, cdr) => Some((car, cdr)),
                _ => None,
            }
        }
    }

    impl PartialEq<X> for X {
        fn eq(&self, rhs: &X) -> bool {
            match (self, rhs) {
                (X::N, X::N) => true,
                (X::I(a), X::I(b)) => a == b,
                (X::P(a, c), X::P(b, d)) => a == b && c == d,
                _ => false,
            }
        }
    }

    impl PartialEq<i64> for X {
        fn eq(&self, rhs: &i64) -> bool {
            match self {
                X::I(i) => i == rhs,
                _ => false,
            }
        }
    }

    impl From<i64> for X {
        fn from(i: i64) -> X {
            X::I(i)
        }
    }
}
