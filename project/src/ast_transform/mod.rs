macro_rules! dispatch {
    ($trans:ident on $node:ident:) => {Visited::Identity};

    ($trans:ident on $node:ident: $kind:ty => $func:expr, $($rest:tt)*) => {
        if let Some(obj) = $node.downcast_ref::<$kind>() {
            Visited::Transformed($func($trans, obj))
        } else {
            dispatch!($trans on $node: $($rest)*)
        }
    };
}

pub mod boxify;
