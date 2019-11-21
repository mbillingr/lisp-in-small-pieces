use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Symbol(Rc<str>);

impl From<&str> for Symbol {
    fn from(s: &str) -> Symbol {
        Symbol(s.into())
    }
}

impl From<Box<str>> for Symbol {
    fn from(s: Box<str>) -> Symbol {
        Symbol(s.into())
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
