use super::DType;

#[derive(Debug, Copy, Clone)]
pub enum Scm {
    Int(i64),
    Flt(f64),
    Vec(&'static Vec<Scm>),
}

impl DType for Scm {
    fn int(i: i64) -> Self {
        Scm::Int(i)
    }
    fn flt(f: f64) -> Self {
        Scm::Flt(f)
    }
    fn vec(v: Vec<Self>) -> Self {
        Scm::Vec(Box::leak(Box::new(v)))
    }

    fn as_int(&self) -> Option<i64> {
        match *self {
            Scm::Int(i) => Some(i),
            _ => None,
        }
    }

    fn as_flt(&self) -> Option<f64> {
        match *self {
            Scm::Flt(i) => Some(i),
            _ => None,
        }
    }

    fn as_vec(&self) -> Option<&Vec<Self>> {
        match self {
            Scm::Vec(i) => Some(i),
            _ => None,
        }
    }

    fn eq(&self, other: &Self) -> bool {
        match (*self, *other) {
            (Scm::Int(a), Scm::Int(b)) => a == b,
            (Scm::Flt(a), Scm::Flt(b)) => a == b,
            (Scm::Vec(a), Scm::Vec(b)) => a as *const Vec<_> == b as *const Vec<_>,
            _ => false,
        }
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (Scm::Int(a), Scm::Int(b)) => a < b,
            (Scm::Flt(a), Scm::Flt(b)) => a < b,
            _ => panic!("invalid comparison"),
        }
    }
}
