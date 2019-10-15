use crate::types::{Scm, Symbol};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    ExpectedSymbol(Scm),
    InvalidForm(Scm),
    NoSuchVariable(Symbol),
}

trait Context: Sized {
    type Meaning;

    fn constant(&mut self, value: Scm) -> Self::Meaning;
    fn shallow_argument_ref(&mut self, j: usize) -> Self::Meaning;
    fn deep_argument_ref(&mut self, i: usize, j: usize) -> Self::Meaning;
    fn checked_global_ref(&mut self, i: usize) -> Self::Meaning;
    fn predefined(&mut self, i: usize) -> Self::Meaning;

    fn meaning(&mut self, e: Scm, r: Environment, is_tail: bool) -> Result<Self::Meaning> {
        if e.is_atom() {
            if e.is_symbol() {
                //self.meaning_reference(e.as_symbol().ok_or_else(||Error::ExpectedSymbol(e))?, r, is_tail)
                unimplemented!()
            } else {
                self.meaning_quotation(e, r, is_tail)
            }
        } else {
            match e.car().unwrap().as_symbol() {
                Some("quote") => self.meaning_quotation(e.cadr().ok_or_else(||Error::InvalidForm(e))?, r, is_tail),
                _ => unimplemented!()
            }
        }
    }

    fn meaning_quotation(&mut self, v: Scm, _r: Environment, _is_tail: bool) -> Result<Self::Meaning> {
        Ok(self.constant(v))
    }
}


/*struct Context {
    g_init: Environment,
    g_current: Environment,
}

impl Context {
    fn new() -> Self {
        Context {
            g_init: Environment::new(),
            g_current: Environment::new(),
        }
    }

    fn meaning<M: Meaning>(&mut self, e: Scm, r: Environment, is_tail: bool) -> Result<M> {
        if e.is_atom() {
            if e.is_symbol() {
                self.meaning_reference(e.as_symbol().ok_or_else(||Error::ExpectedSymbol(e))?, r, is_tail)
            } else {
                self.meaning_quotation(e, r, is_tail)
            }
        } else {
            unimplemented!()
        }
    }

    fn meaning_quotation<M: Meaning>(&mut self, v: Scm, _r: Environment, _is_tail: bool) -> Result<M> {
        Ok(M::constant(v))
    }

    fn meaning_reference<M: Meaning>(&mut self, n: Symbol, r: Environment, _is_tail: bool) -> Result<M> {
        match self.find_variable(r, n) {
            Address::None => Err(Error::NoSuchVariable(n)),
            Address::Local(i, j) if i == 0 => Ok(M::shallow_argument_ref(j)),
            Address::Local(i, j) => Ok(M::deep_argument_ref(i, j)),
            Address::Global(i) => Ok(M::checked_global_ref(i)),
            Address::Static(i) => Ok(M::predefined(i)),
        }
    }

    fn find_variable(&self, r: Environment, n: Symbol) -> Address {
        r.find(n).map(|(i, j)| Address::Local(i, j))
            .or_else(|| self.g_current.find(n).map(|(_, i)| Address::Global(i)))
            .or_else(|| self.g_init.find(n).map(|(_, i)| Address::Static(i)))
            .unwrap_or(Address::None)
    }
}*/

enum Address {
    None,
    Local(usize, usize),
    Global(usize),
    Static(usize),
}

struct Environment {

}

impl Environment {
    fn new() -> Self {
        Environment {
        }
    }

    fn find(&self, name: Symbol) -> Option<(usize, usize)> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lisp_core::lexpr;

    #[derive(Debug, Copy, Clone)]
    struct MockContext {

    }

    impl MockContext {
        fn new() -> Self {
            MockContext {}
        }
    }

    impl Context for MockContext {
        type Meaning = Scm;

        fn constant(&mut self, value: Scm) -> Self::Meaning {
            value
        }

        fn shallow_argument_ref(&mut self, j: usize) -> Self::Meaning {
            unimplemented!()
        }

        fn deep_argument_ref(&mut self, i: usize, j: usize) -> Self::Meaning {
            unimplemented!()
        }

        fn checked_global_ref(&mut self, i: usize) -> Self::Meaning {
            unimplemented!()
        }

        fn predefined(&mut self, i: usize) -> Self::Meaning {
            unimplemented!()
        }
    }

    #[test]
    fn self_evaluating() {
        let mut ctx = MockContext::new();

        let expr = lexpr::from_str("42").unwrap().into();
        let m = ctx.meaning(expr, Environment::new(), true).unwrap();

        assert_eq!(m, expr);
    }

    #[test]
    fn self_quotation() {
        let mut ctx = MockContext::new();

        let expr = lexpr::from_str("(quote (x 42))").unwrap().into();
        let m = ctx.meaning(expr, Environment::new(), true).unwrap();

        let expect = lexpr::from_str("(x 42)").unwrap().into();
        assert!(m.equal(&expect));
    }
}