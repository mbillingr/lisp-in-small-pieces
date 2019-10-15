use super::{
    scm::{
        Scm, SPECIAL_FALSE, SPECIAL_NULL, SPECIAL_TRUE, SPECIAL_UNINIT, TAG_INTEGER, TAG_MASK,
        TAG_PAIR, TAG_POINTER,
    },
    ScmBoxedValue,
};

impl std::fmt::Display for Scm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match (self.ptr, self.ptr & TAG_MASK) {
            (SPECIAL_UNINIT, _) => write!(f, "*uninit*"),
            (SPECIAL_NULL, _) => write!(f, "'()"),
            (SPECIAL_FALSE, _) => write!(f, "#f"),
            (SPECIAL_TRUE, _) => write!(f, "'#t"),
            (_, TAG_INTEGER) => write!(f, "{}", self.as_int().unwrap()),
            (_, TAG_PAIR) => {
                let (car, cdr) = self.as_pair().unwrap();
                write!(f, "({} . {})", car, cdr)
            }
            (_, TAG_POINTER) => write!(f, "{}", unsafe { self.deref() }),
            (_, _) => write!(f, "*invalid*"),
        }
    }
}

impl std::fmt::Display for ScmBoxedValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ScmBoxedValue::Symbol(s) => write!(f, "{}", s),
            ScmBoxedValue::String(s) => write!(f, "\"{}\"", s),
            ScmBoxedValue::Primitive(p) => write!(f, "<primitive {}>", p.name()),
            ScmBoxedValue::Frame(af) => write!(f, "{:?}", af),
            ScmBoxedValue::Closure(c) => write!(f, "{:?}", c),
            ScmBoxedValue::Escape(e) => write!(f, "{:?}", e),
            //Value::Pointer(p) => write!(f, "{:p}", p),
        }
    }
}
