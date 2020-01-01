use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::source::SourceLocation;

#[derive(Debug, Clone)]
pub struct Constant {
    pub value: Sexpr,
    span: SourceLocation,
}

impl Constant {
    pub fn new(value: Sexpr, span: SourceLocation) -> Self {
        Constant { value, span }
    }
}

impl From<TrackedSexpr> for Constant {
    fn from(sexpr: TrackedSexpr) -> Self {
        Constant {
            value: sexpr.sexpr,
            span: sexpr.src,
        }
    }
}

impl From<Sexpr> for Constant {
    fn from(sexpr: Sexpr) -> Self {
        Constant {
            value: sexpr,
            span: SourceLocation::NoSource,
        }
    }
}
