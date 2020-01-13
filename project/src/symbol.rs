use std::collections::HashSet;
use std::sync::RwLock;

use lazy_static::lazy_static;

lazy_static! {
    static ref INTERNED_STRINGS: RwLock<HashSet<&'static str>> = RwLock::new(HashSet::new());
}

#[derive(Copy, Clone, Hash)]
pub struct Symbol(&'static str);

impl Symbol {
    pub fn new(s: &'static str) -> Self {
        Self::interned(s, |s| s)
    }

    pub fn from_str(s: &str) -> Self {
        Self::interned(s, |s| Box::leak(Box::new(s.to_owned())))
    }

    pub fn from_string(s: String) -> Self {
        Self::interned(s, |s| Box::leak(Box::new(s)))
    }

    pub fn from_boxed_str(s: Box<str>) -> Self {
        Self::interned(s, |s| Box::leak(s))
    }

    fn interned<T: AsRef<str>>(s: T, into_static: impl Fn(T) -> &'static str) -> Self {
        if let Some(is) = INTERNED_STRINGS
            .read()
            .expect("could not get read lock on interned strings")
            .get(s.as_ref())
        {
            return Symbol(is);
        }
        let mut lock = INTERNED_STRINGS
            .write()
            .expect("could not get write lock on interned strings");
        if let Some(is) = lock.get(s.as_ref()) {
            // a different thread may have inserted the symbol while we were waiting for the lock...
            return Symbol(is);
        } else {
            let s = into_static(s);
            lock.insert(s);
            Symbol(s)
        }
    }

    pub const fn uninterned(s: &'static str) -> Self {
        Symbol(s)
    }

    pub fn as_uninterned(&self) -> Self {
        let s = Box::leak(Box::new(self.0.to_owned()));
        Symbol(s)
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.0 as *const str == other.0 as *const str
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.ptr_eq(other)
    }
}

impl Eq for Symbol {}

impl From<&str> for Symbol {
    fn from(s: &str) -> Symbol {
        Symbol::from_str(s)
    }
}

impl From<Box<str>> for Symbol {
    fn from(s: Box<str>) -> Symbol {
        Symbol::from_boxed_str(s)
    }
}

impl std::cmp::PartialEq<str> for Symbol {
    fn eq(&self, s: &str) -> bool {
        s.eq(&*self.0)
    }
}

impl std::cmp::PartialEq<Symbol> for str {
    fn eq(&self, s: &Symbol) -> bool {
        self.eq(&*s.0)
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "({} {:p})", self.0, self.0)
        } else {
            write!(f, "Symbol({} {:p})", self.0, self.0)
        }
    }
}
