use crate::types::{Scm, Symbol};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    ExpectedSymbol(Scm),
    ExpectedList(Scm),
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
    fn alternative(
        &mut self,
        m_cond: Self::Meaning,
        m_then: Self::Meaning,
        m_else: Self::Meaning,
    ) -> Self::Meaning;
    fn sequence(
        &mut self,
        first: Self::Meaning,
        next: Self::Meaning,
    ) -> Self::Meaning;
    fn fix_closure(
        &mut self,
        body: Self::Meaning,
        artity: usize,
    ) -> Self::Meaning;

    fn g_init(&self) -> &Environment;
    fn g_current(&self) -> &Environment;

    fn meaning(&mut self, e: Scm, r: &'static Environment, is_tail: bool) -> Result<Self::Meaning> {
        if e.is_atom() {
            if e.is_symbol() {
                self.meaning_reference(
                    e.as_symbol().ok_or_else(|| Error::ExpectedSymbol(e))?,
                    r,
                    is_tail,
                )
            } else {
                self.meaning_quotation(e, r, is_tail)
            }
        } else {
            match e.car().unwrap().as_symbol() {
                Some("quote") => self.meaning_quotation(
                    e.cadr().ok_or_else(|| Error::InvalidForm(e))?,
                    r,
                    is_tail,
                ),
                Some("lambda") => self.meaning_abstraction(
                    e.cadr().ok_or_else(|| Error::InvalidForm(e))?,
                    e.cddr().ok_or_else(|| Error::InvalidForm(e))?,
                    r,
                    is_tail,
                ),
                Some("if") => self.meaning_alternative(
                    e.cadr().ok_or_else(|| Error::InvalidForm(e))?,
                    e.caddr().ok_or_else(|| Error::InvalidForm(e))?,
                    e.cadddr().ok_or_else(|| Error::InvalidForm(e))?,
                    r,
                    is_tail,
                ),
                _ => unimplemented!(),
            }
        }
    }

    fn meaning_quotation(
        &mut self,
        v: Scm,
        _r: &Environment,
        _is_tail: bool,
    ) -> Result<Self::Meaning> {
        Ok(self.constant(v))
    }

    fn meaning_reference(
        &mut self,
        n: Symbol,
        r: &Environment,
        _is_tail: bool,
    ) -> Result<Self::Meaning> {
        match self.find_variable(r, n) {
            Address::None => Err(Error::NoSuchVariable(n)),
            Address::Local(i, j) if i == 0 => Ok(self.shallow_argument_ref(j)),
            Address::Local(i, j) => Ok(self.deep_argument_ref(i, j)),
            Address::Global(i) => Ok(self.checked_global_ref(i)),
            Address::Static(i) => Ok(self.predefined(i)),
        }
    }

    fn meaning_alternative(
        &mut self,
        e_cond: Scm,
        e_then: Scm,
        e_else: Scm,
        r: &'static Environment,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        let m_cond = self.meaning(e_cond, r, false)?;
        let m_then = self.meaning(e_then, r, is_tail)?;
        let m_else = self.meaning(e_else, r, is_tail)?;
        Ok(self.alternative(m_cond, m_then, m_else))
    }

    fn meaning_sequence(&mut self, es: Scm, r: &'static Environment, is_tail: bool) -> Result<Self::Meaning> {
        let &(first, rest) = es.as_pair().ok_or_else(||Error::ExpectedList(es))?;
        if rest.is_pair() {
            self.meaning_multiple_sequence(first, rest, r, is_tail)
        } else {
            self.meaning(first, r, is_tail)
        }
    }

    fn meaning_multiple_sequence(&mut self, e: Scm, es: Scm, r: &'static Environment, is_tail: bool) -> Result<Self::Meaning> {
        let m = self.meaning(e, r, false)?;
        let ms = self.meaning_sequence(es, r, is_tail)?;
        Ok(self.sequence(m, ms))
    }

    fn meaning_abstraction(
        &mut self,
        names: Scm,
        body: Scm,
        r: &'static Environment,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        let mut ns = names;
        let mut regular = Scm::null();
        loop {
            match 0 {
                _ if ns.is_pair() => {
                    ns = ns.cdr().unwrap();
                    regular = Scm::cons(ns, regular);
                }
                _ if ns.is_null() => return self.meaning_fix_abstraction(names, body, r, is_tail),
                _ => {
                    return self.meaning_dotted_abstraction(
                        regular
                            .reverse()
                            .unwrap(),
                        ns,
                        body,
                        r,
                        is_tail,
                    )
                }
            }
        }
    }

    fn meaning_fix_abstraction(
        &mut self,
        names: Scm,
        body: Scm,
        r: &'static Environment,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        let arity = names.len().ok_or_else(||Error::ExpectedList(names))?;
        let r2 = r.extend(names);
        let m_body = self.meaning_sequence(body, &r2, true)?;
        Ok(self.fix_closure(m_body, arity))
    }

    fn meaning_dotted_abstraction(
        &mut self,
        names: Scm,
        extra: Scm,
        body: Scm,
        r: &Environment,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        unimplemented!()
    }

    fn find_variable(&self, r: &Environment, n: Symbol) -> Address {
        r.find(n)
            .map(|(i, j)| Address::Local(i, j))
            .or_else(|| self.g_current().find(n).map(|(_, i)| Address::Global(i)))
            .or_else(|| self.g_init().find(n).map(|(_, i)| Address::Static(i)))
            .unwrap_or(Address::None)
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

#[derive(Debug)]
struct Environment {
    next: Option<&'static Environment>,
    names: Vec<Symbol>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            next: None,
            names: vec![],
        }
    }

    fn allocate() -> &'static mut Self {
        Box::leak(Box::new(Self::new()))
    }

    fn extend(&'static self, mut vars: Scm) -> &'static Self {
        let next = Some(self);
        let mut names = vec![];
        loop {
            if vars.is_null() {
                break
            }
            let &(n, rest) = vars.as_pair().unwrap();
            names.push(n.as_symbol().unwrap());
            vars = rest;
        }
        Box::leak(Box::new(Environment {
            next, names
        }))
    }

    fn find(&self, name: Symbol) -> Option<(usize, usize)> {
        let mut env = Some(self);
        let mut i = 0;
        while let Some(r) = env {
            for (j, &n) in r.names.iter().enumerate() {
                if n == name {
                    return Some((i, j));
                }
            }
            i += 1;
            env = r.next;
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::ActivationFrame;
    use lisp_core::lexpr;

    #[derive(Debug)]
    struct MockContext {
        g_init: Environment,
        g_current: Environment,

        sg_init: Vec<Scm>,
        sg_current: Vec<Scm>,
        r_env: &'static ActivationFrame,
    }

    impl MockContext {
        fn new() -> Self {
            let mut mc = MockContext {
                g_init: Environment::new(),
                g_current: Environment::new(),

                sg_init: vec![Scm::string("I am FOO"), Scm::string("I am BAR")],
                sg_current: vec![],
                r_env: ActivationFrame::allocate(0),
            };
            mc.g_init.names = vec!["foo".into(), "bar".into()];
            mc
        }
    }

    impl Context for MockContext {
        type Meaning = Scm;

        fn g_current(&self) -> &Environment {
            &self.g_current
        }

        fn g_init(&self) -> &Environment {
            &self.g_init
        }

        fn constant(&mut self, value: Scm) -> Self::Meaning {
            value
        }

        fn shallow_argument_ref(&mut self, j: usize) -> Self::Meaning {
            *self.r_env.argument(j)
        }

        fn deep_argument_ref(&mut self, i: usize, j: usize) -> Self::Meaning {
            *self.r_env.deep_fetch(i, j)
        }

        fn checked_global_ref(&mut self, i: usize) -> Self::Meaning {
            unimplemented!()
        }

        fn predefined(&mut self, i: usize) -> Self::Meaning {
            self.sg_init[i]
        }

        fn alternative(
            &mut self,
            m_cond: Self::Meaning,
            m_then: Self::Meaning,
            m_else: Self::Meaning,
        ) -> Self::Meaning {
            if m_cond.as_bool() {
                m_then
            } else {
                m_else
            }
        }

        fn sequence(
            &mut self,
            first: Self::Meaning,
            next: Self::Meaning,
        ) -> Self::Meaning {
            next
        }

        fn fix_closure(
            &mut self,
            body: Self::Meaning,
            artity: usize,
        ) -> Self::Meaning {
            unimplemented!()
        }
    }

    #[test]
    fn self_evaluating() {
        let mut ctx = MockContext::new();

        let expr = lexpr::from_str("42").unwrap().into();
        let m = ctx.meaning(expr, Environment::allocate(), true).unwrap();

        assert_eq!(m, expr);
    }

    #[test]
    fn quotation() {
        let mut ctx = MockContext::new();

        let expr = lexpr::from_str("(quote (x 42))").unwrap().into();
        let m = ctx.meaning(expr, Environment::allocate(), true).unwrap();

        let expect = lexpr::from_str("(x 42)").unwrap().into();
        assert!(m.equal(&expect));
    }

    #[test]
    fn shallow_ref() {
        let mut ctx = MockContext::new();
        ctx.r_env = ActivationFrame::allocate(3);
        ctx.r_env.set_argument(0, Scm::int(1));
        ctx.r_env.set_argument(1, Scm::int(42));
        ctx.r_env.set_argument(2, Scm::int(3));

        let mut env = Environment::allocate();
        env.names = vec!["x".into(), "y".into(), "z".into()];

        let expr = lexpr::from_str("y").unwrap().into();
        let m = ctx.meaning(expr, env, true).unwrap();

        assert_eq!(m, Scm::int(42));
    }

    #[test]
    fn deep_ref() {
        let mut ctx = MockContext::new();
        ctx.r_env = ActivationFrame::allocate(3);
        ctx.r_env.set_argument(0, Scm::int(1));
        ctx.r_env.set_argument(1, Scm::int(42));
        ctx.r_env.set_argument(2, Scm::int(3));

        let r_env2 = ActivationFrame::allocate(3);
        ctx.r_env = r_env2.extends(&ctx.r_env);

        let mut env1 = Environment::allocate();
        env1.names = vec!["x".into(), "y".into(), "z".into()];
        let mut env2 = Environment::allocate();
        env2.names = vec!["a".into(), "b".into(), "c".into()];
        env2.next = Some(env1);

        let expr = lexpr::from_str("y").unwrap().into();
        let m = ctx.meaning(expr, env2, true).unwrap();

        assert_eq!(m, Scm::int(42));
    }

    #[test]
    fn predefined_ref() {
        let mut ctx = MockContext::new();

        let expr = lexpr::from_str("foo").unwrap().into();
        let m = ctx.meaning(expr, Environment::allocate(), true).unwrap();

        assert_eq!(m.as_string().unwrap(), "I am FOO");
    }

    #[test]
    fn alternative() {
        let mut ctx = MockContext::new();
        let env = Environment::allocate();

        let expr = lexpr::from_str("(if #t 1 2)").unwrap().into();
        let m = ctx.meaning(expr, env, true).unwrap();
        assert_eq!(m, Scm::int(1));

        let expr = lexpr::from_str("(if 0 1 2)").unwrap().into();
        let m = ctx.meaning(expr, env, true).unwrap();
        assert_eq!(m, Scm::int(1));

        let expr = lexpr::from_str("(if #f 1 2)").unwrap().into();
        let m = ctx.meaning(expr, env, true).unwrap();
        assert_eq!(m, Scm::int(2));
    }

    #[test]
    fn abstraction() {
        let mut ctx = MockContext::new();

        let expr = lexpr::from_str("(lambda (x) x)").unwrap().into();
        let m = ctx.meaning(expr, Environment::allocate(), true).unwrap();
        assert_eq!(m, Scm::int(1));
    }
}
