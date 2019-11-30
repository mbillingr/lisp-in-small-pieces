use super::DType;

pub type Scm = &'static ScmValue;

#[derive(Debug, Clone)]
pub enum ScmValue {
    Int(i64),
    Flt(f64),
    Vec(Vec<Scm>),
}

impl DType for Scm {
    fn int(i: i64) -> Self {
        Box::leak(Box::new(ScmValue::Int(i)))
    }
    fn flt(f: f64) -> Self {
        Box::leak(Box::new(ScmValue::Flt(f)))
    }
    fn vec(v: Vec<Self>) -> Self {
        Box::leak(Box::new(ScmValue::Vec(v)))
    }

    fn as_int(&self) -> Option<i64> {
        match **self {
            ScmValue::Int(i) => Some(i),
            _ => None,
        }
    }

    fn as_flt(&self) -> Option<f64> {
        match **self {
            ScmValue::Flt(i) => Some(i),
            _ => None,
        }
    }

    fn as_vec(&self) -> Option<&Vec<Self>> {
        match *self {
            ScmValue::Vec(i) => Some(i),
            _ => None,
        }
    }

    fn eq(&self, other: &Self) -> bool {
        *self as *const ScmValue == *other as *const ScmValue
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (ScmValue::Int(a), ScmValue::Int(b)) => a < b,
            (ScmValue::Flt(a), ScmValue::Flt(b)) => a < b,
            _ => panic!("invalid comparison"),
        }
    }
}
