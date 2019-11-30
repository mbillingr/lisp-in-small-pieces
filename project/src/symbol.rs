use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;
use std::sync::RwLock;

use lazy_static::lazy_static;

lazy_static! {
    static ref INTERNED_STRINGS: RwLock<HashSet<&'static str>> = RwLock::new(HashSet::new());
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Symbol(&'static str);

impl Symbol {
    pub fn new(s: &'static str) -> Self {
        if let Some(is) = INTERNED_STRINGS
            .read()
            .expect("could not get read lock on interned strings")
            .get(s)
        {
            return Symbol(is);
        }
        INTERNED_STRINGS
            .write()
            .expect("could not get write lock on interned strings")
            .insert(s);
        Symbol(s)
    }

    pub fn from_string(s: String) -> Self {
        if let Some(is) = INTERNED_STRINGS
            .read()
            .expect("could not get read lock on interned strings")
            .get(s.as_str())
        {
            return Symbol(is);
        }
        let s = Box::leak(Box::new(s));
        INTERNED_STRINGS
            .write()
            .expect("could not get write lock on interned strings")
            .insert(s);
        Symbol(s)
    }

    pub fn from_boxed_str(s: Box<str>) -> Self {
        if let Some(is) = INTERNED_STRINGS
            .read()
            .expect("could not get read lock on interned strings")
            .get(&*s)
        {
            return Symbol(is);
        }
        let s = Box::leak(s);
        INTERNED_STRINGS
            .write()
            .expect("could not get write lock on interned strings")
            .insert(s);
        Symbol(s)
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.0 as *const str == other.0 as *const str
    }
}

impl From<&str> for Symbol {
    fn from(s: &str) -> Symbol {
        Symbol::from_string(s.to_string())
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
