use std::rc::Rc;

pub enum Sexpr {
    Symbol(Rc<String>),

    List(RcSlice<Sexpr>)

}

impl Sexpr {
    pub fn first(&self) -> Option<&Self> {
        match self {
            Sexpr::List(l) => Some(&l[0]),
            _ => None,
        }
    }

    pub fn tail(&self) -> Option<Self> {
        match self {
            Sexpr::List(l) if l.len() > 0 => {
                Some(Sexpr::List(l.slice_from(1)))
            },
            _ => None,
        }
    }

    pub fn at(&self, idx: usize) -> Option<&Self> {
        match self {
            Sexpr::List(l) if l.len() > idx => {
                Some(&l[idx])
            },
            _ => None,
        }
    }
}

impl From<lexpr::Value> for Sexpr {
    fn from(x: lexpr::Value) -> Self {
        unimplemented!()
    }
}


struct RcSlice<T> {
    base: Rc<[T]>,
    slice: *const [T],
}

impl<T:std::fmt::Debug> std::fmt::Debug for RcSlice<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", &**self)
    }
}

impl<T> Clone for RcSlice<T> {
    fn clone(&self) -> Self {
        RcSlice {
            base: self.base.clone(),
            slice: self.slice,
        }
    }
}

impl<T> From<Vec<T>> for RcSlice<T> {
    fn from(v: Vec<T>) -> Self {
        let src: Rc<_> = v.into();
        RcSlice {
            slice: &*src,
            base: src,
        }
    }

}

impl<T> std::ops::Deref for RcSlice<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe{&*self.slice}
    }
}

impl<T> RcSlice<T> {
    pub fn slice(self, from: usize, to: usize) -> Self {
        RcSlice {
            base: self.base,
            slice: unsafe { &(*self.slice)[from..to] },
        }
    }

    pub fn slice_to(self, to: usize) -> Self {
        self.slice(0, to)
    }

    pub fn slice_from(self, from: usize) -> Self {
        let n = self.len();
        self.slice(from, n)
    }
}
