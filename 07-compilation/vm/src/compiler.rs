use crate::types::{Scm, Symbol};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    ExpectedSymbol(Scm),
    ExpectedList(Scm),
    InvalidForm(Scm),
    NoSuchVariable(Symbol),
    ImmutableVariable(Symbol),
    IncorrectArity(Scm, usize),
}

trait Context: Sized {
    type Meaning;

    fn constant(&mut self, value: Scm) -> Self::Meaning;
    fn shallow_argument_ref(&mut self, j: usize) -> Self::Meaning;
    fn deep_argument_ref(&mut self, i: usize, j: usize) -> Self::Meaning;
    fn checked_global_ref(&mut self, i: usize) -> Self::Meaning;
    fn shallow_argument_set(&mut self, j: usize, m: Self::Meaning) -> Self::Meaning;
    fn deep_argument_set(&mut self, i: usize, j: usize, m: Self::Meaning) -> Self::Meaning;
    fn global_set(&mut self, i: usize, m: Self::Meaning) -> Self::Meaning;
    fn predefined(&mut self, i: usize) -> Self::Meaning;
    fn alternative(
        &mut self,
        m_cond: Self::Meaning,
        m_then: Self::Meaning,
        m_else: Self::Meaning,
    ) -> Self::Meaning;
    fn sequence(&mut self, first: Self::Meaning, next: Self::Meaning) -> Self::Meaning;
    fn fix_closure(&mut self, body: Self::Meaning, arity: usize) -> Self::Meaning;
    fn store_argument(
        &mut self,
        arg: Self::Meaning,
        args: Self::Meaning,
        i: usize,
    ) -> Self::Meaning;
    fn allocate_frame(&mut self, size: usize) -> Self::Meaning;
    fn tr_regular_call(&mut self, func: Self::Meaning, args: Self::Meaning) -> Self::Meaning;
    fn regular_call(&mut self, func: Self::Meaning, args: Self::Meaning) -> Self::Meaning;

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
                Some("begin") => {
                    self.meaning_sequence(e.cdr().ok_or_else(|| Error::InvalidForm(e))?, r, is_tail)
                }
                Some("set!") => self.meaning_assignment(
                    e.cadr()
                        .ok_or_else(|| Error::InvalidForm(e))?
                        .as_symbol()
                        .ok_or_else(|| Error::ExpectedSymbol(e))?,
                    e.caddr().ok_or_else(|| Error::InvalidForm(e))?,
                    r,
                    is_tail,
                ),
                _ => self.meaning_application(
                    e.car().unwrap(),
                    e.cdr().ok_or_else(|| Error::InvalidForm(e))?,
                    r,
                    is_tail,
                ),
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

    fn meaning_assignment(
        &mut self,
        n: Symbol,
        e: Scm,
        r: &'static Environment,
        _is_tail: bool,
    ) -> Result<Self::Meaning> {
        let m = self.meaning(e, r, false)?;
        match self.find_variable(r, n) {
            Address::None => Err(Error::NoSuchVariable(n)),
            Address::Local(i, j) if i == 0 => Ok(self.shallow_argument_set(j, m)),
            Address::Local(i, j) => Ok(self.deep_argument_set(i, j, m)),
            Address::Global(i) => Ok(self.global_set(i, m)),
            Address::Static(i) => Err(Error::ImmutableVariable(n)),
        }
    }

    fn meaning_sequence(
        &mut self,
        es: Scm,
        r: &'static Environment,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        let &(first, rest) = es.as_pair().ok_or_else(|| Error::ExpectedList(es))?;
        if rest.is_pair() {
            self.meaning_multiple_sequence(first, rest, r, is_tail)
        } else {
            self.meaning(first, r, is_tail)
        }
    }

    fn meaning_multiple_sequence(
        &mut self,
        e: Scm,
        es: Scm,
        r: &'static Environment,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
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
                        regular.reverse().unwrap(),
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
        let arity = names.len().ok_or_else(|| Error::ExpectedList(names))?;
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

    fn meaning_application(
        &mut self,
        func: Scm,
        args: Scm,
        r: &'static Environment,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        if let Some(name) = func.as_symbol() {
            if let Address::Static(i) = self.find_variable(r, name) {
                let desc = self.get_description(name);
                if desc.is_function() {
                    let arity = args.len().ok_or_else(|| Error::ExpectedList(args))?;
                    if desc.check_arity(arity) {
                        return self.meaning_primitive_application(name, args, r, is_tail);
                    } else {
                        return Err(Error::IncorrectArity(func, arity));
                    }
                }
            }
        }

        if let Some(fcar) = func.car() {
            if let Some("lambda") = fcar.as_symbol() {
                return self.meaning_closed_application(func, args, r, is_tail);
            }
        }

        self.meaning_regular_application(func, args, r, is_tail)
    }

    fn meaning_regular_application(
        &mut self,
        func: Scm,
        args: Scm,
        r: &'static Environment,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        let m_func = self.meaning(func, r, false)?;
        let m_args = self.meaning_arguments(
            args,
            r,
            args.len().ok_or_else(|| Error::ExpectedList(args))?,
            false,
        )?;
        if is_tail {
            Ok(self.tr_regular_call(m_func, m_args))
        } else {
            Ok(self.regular_call(m_func, m_args))
        }
    }

    fn meaning_closed_application(
        &mut self,
        func: Scm,
        args: Scm,
        r: &'static Environment,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        unimplemented!()
    }

    fn meaning_primitive_application(
        &mut self,
        name: Symbol,
        args: Scm,
        r: &'static Environment,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        unimplemented!()
    }

    fn meaning_arguments(
        &mut self,
        args: Scm,
        r: &'static Environment,
        size: usize,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        if let Some(&(car, cdr)) = args.as_pair() {
            self.meaning_more_arguments(car, cdr, r, size, is_tail)
        } else {
            self.meaning_no_more_arguments(r, size, is_tail)
        }
    }

    fn meaning_more_arguments(
        &mut self,
        arg: Scm,
        args: Scm,
        r: &'static Environment,
        size: usize,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        let m = self.meaning(arg, r, false)?;
        let more = self.meaning_arguments(args, r, size, is_tail)?;
        let rank = size - args.len().unwrap() - 1;
        Ok(self.store_argument(m, more, rank))
    }

    fn meaning_no_more_arguments(
        &mut self,
        r: &'static Environment,
        size: usize,
        is_tail: bool,
    ) -> Result<Self::Meaning> {
        Ok(self.allocate_frame(size))
    }

    fn meaning_dynamic_reference(&mut self, name: Symbol, r: &'static Environment,
                                 is_tail: bool,
    ) -> Result<Self::Meaning> {
        unimplemented!()
    }

    fn meaning_dynamic_let(&mut self,
                           name: Symbol,
                           value: Scm,
                           body: Scm,
                           r: &'static Environment,
                           is_tail: bool,
    ) -> Result<Self::Meaning> {
        unimplemented!()
    }

    fn meaning_bind_exit(&mut self,
                         name: Symbol,
                         body: Scm,
                         r: &'static Environment,
                         is_tail: bool,
    ) -> Result<Self::Meaning> {
        unimplemented!()
    }

    fn meaning_monitor(&mut self,
                       handler: Scm,
                       body: Scm,
                       r: &'static Environment,
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

    fn get_description(&self, n: Symbol) -> Description {
        unimplemented!()
    }
}

enum Address {
    None,
    Local(usize, usize),
    Global(usize),
    Static(usize),
}

enum Description {}

impl Description {
    fn is_function(&self) -> bool {
        unimplemented!()
    }

    fn check_arity(&self, arity: usize) -> bool {
        unimplemented!()
    }
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
                break;
            }
            let &(n, rest) = vars.as_pair().unwrap();
            names.push(n.as_symbol().unwrap());
            vars = rest;
        }
        Box::leak(Box::new(Environment { next, names }))
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
    use crate::types::scm_boxed_value::UserValue;
    use crate::types::{ActivationFrame, Primitive, ScmBoxedValue};
    use crate::vm::VirtualMachine;
    use lisp_core::lexpr;

    fn make_static<T>(value: T) -> &'static T {
        Box::leak(Box::new(value))
    }

    struct Closure {
        code: &'static dyn Fn(&mut VirtualMachine, &'static ActivationFrame),
        closed_env: &'static ActivationFrame,
    }

    impl std::fmt::Display for Closure {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "*CLOSURE*")
        }
    }

    impl std::fmt::Debug for Closure {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "*CLOSURE*")
        }
    }

    impl Closure {
        fn invoke(&self, state: &mut VirtualMachine) {
            (self.code)(state, self.closed_env)
        }
    }

    #[derive(Debug)]
    struct MockContext {
        g_init: Environment,
        g_current: Environment,
    }

    impl MockContext {
        fn new() -> Self {
            let mut mc = MockContext {
                g_init: Environment::new(),
                g_current: Environment::new(),
            };
            mc.g_init.names = vec!["foo".into(), "bar".into()];
            mc
        }
    }

    impl Context for MockContext {
        type Meaning = &'static dyn Fn(&mut VirtualMachine);

        fn g_current(&self) -> &Environment {
            &self.g_current
        }

        fn g_init(&self) -> &Environment {
            &self.g_init
        }

        fn constant(&mut self, value: Scm) -> Self::Meaning {
            make_static(move |state| state.val = value)
        }

        fn shallow_argument_ref(&mut self, j: usize) -> Self::Meaning {
            make_static(move |state| state.val = *state.env.argument(j))
        }

        fn deep_argument_ref(&mut self, i: usize, j: usize) -> Self::Meaning {
            make_static(move |state| state.val = *state.env.deep_fetch(i, j))
        }

        fn checked_global_ref(&mut self, i: usize) -> Self::Meaning {
            make_static(move |state| state.val = state.mut_globals[i])
        }

        fn shallow_argument_set(&mut self, j: usize, mut m: Self::Meaning) -> Self::Meaning {
            make_static(move |state| {
                m(state);
                state.env.set_argument(j, state.val);
            })
        }

        fn deep_argument_set(&mut self, i: usize, j: usize, mut m: Self::Meaning) -> Self::Meaning {
            make_static(move |state| {
                m(state);
                state.env.deep_set(i, j, state.val);
            })
        }

        fn global_set(&mut self, i: usize, mut m: Self::Meaning) -> Self::Meaning {
            make_static(move |state| {
                m(state);
                state.mut_globals[i] = state.val;
            })
        }

        fn predefined(&mut self, i: usize) -> Self::Meaning {
            make_static(move |state| state.val = state.globals[i])
        }

        fn alternative(
            &mut self,
            mut m_cond: Self::Meaning,
            mut m_then: Self::Meaning,
            mut m_else: Self::Meaning,
        ) -> Self::Meaning {
            make_static(move |state| {
                m_cond(state);
                if state.val.as_bool() {
                    m_then(state)
                } else {
                    m_else(state)
                }
            })
        }

        fn sequence(&mut self, mut first: Self::Meaning, mut next: Self::Meaning) -> Self::Meaning {
            make_static(move |state| {
                first(state);
                next(state);
            })
        }

        fn fix_closure(&mut self, mut body: Self::Meaning, arity: usize) -> Self::Meaning {
            let arityp1 = arity + 1;
            make_static(move |state| {
                let the_function =
                    move |state: &mut VirtualMachine, env: &'static ActivationFrame| {
                        let frame = state.val.as_frame().expect("Expected Activation Frame");
                        if frame.len() == arityp1 {
                            state.env = frame.extends(env);
                            body(state);
                        } else {
                            panic!("Incorrect arity")
                        }
                    };
                //state.val = Self::make_closure(make_static(the_function), state.env);
                let closure = Closure {
                    code: Box::leak(Box::new(the_function)),
                    closed_env: state.env,
                };
                let closure: &dyn UserValue = Box::leak(Box::new(closure));
                state.val = closure.as_scm();
            })
        }

        fn store_argument(
            &mut self,
            mut arg: Self::Meaning,
            mut args: Self::Meaning,
            i: usize,
        ) -> Self::Meaning {
            make_static(move |state| {
                arg(state);
                state.stack_push(state.val);
                args(state);
                let v = unsafe { state.stack_pop_into() };
                state.val.as_frame().unwrap().set_argument(i, v);
            })
        }

        fn allocate_frame(&mut self, size: usize) -> Self::Meaning {
            let sizep1 = size + 1;
            make_static(move |state| state.val = Scm::frame(sizep1))
        }

        fn tr_regular_call(
            &mut self,
            mut func: Self::Meaning,
            mut args: Self::Meaning,
        ) -> Self::Meaning {
            make_static(move |state| {
                func(state);
                state.stack_push(state.val);
                args(state);
                state.fun = unsafe { state.stack_pop_into() };
                state.fun.as_user_value::<Closure>().unwrap().invoke(state);
            })
        }

        fn regular_call(&mut self, func: Self::Meaning, args: Self::Meaning) -> Self::Meaning {
            unimplemented!()
        }
    }

    #[test]
    fn self_evaluating() {
        let mut ctx = MockContext::new();

        let expr = lexpr::from_str("42").unwrap().into();
        let mut m = ctx.meaning(expr, Environment::allocate(), true).unwrap();

        let vm = &mut VirtualMachine::default();
        m(vm);
        assert_eq!(vm.val, expr);
    }

    #[test]
    fn quotation() {
        let mut ctx = MockContext::new();

        let expr = lexpr::from_str("(quote (x 42))").unwrap().into();
        let mut m = ctx.meaning(expr, Environment::allocate(), true).unwrap();

        let expect = lexpr::from_str("(x 42)").unwrap().into();
        let vm = &mut VirtualMachine::default();
        m(vm);
        assert!(vm.val.equal(&expect));
    }

    #[test]
    fn shallow_ref() {
        let mut ctx = MockContext::new();

        let vm = &mut VirtualMachine::default();
        vm.env = ActivationFrame::allocate(3);
        vm.env.set_argument(0, Scm::int(1));
        vm.env.set_argument(1, Scm::int(42));
        vm.env.set_argument(2, Scm::int(3));

        let mut env = Environment::allocate();
        env.names = vec!["x".into(), "y".into(), "z".into()];

        let expr = lexpr::from_str("y").unwrap().into();
        let mut m = ctx.meaning(expr, env, true).unwrap();

        m(vm);
        assert_eq!(vm.val, Scm::int(42));
    }

    #[test]
    fn deep_ref() {
        let mut ctx = MockContext::new();

        let vm = &mut VirtualMachine::default();
        vm.env = ActivationFrame::allocate(3);
        vm.env.set_argument(0, Scm::int(1));
        vm.env.set_argument(1, Scm::int(42));
        vm.env.set_argument(2, Scm::int(3));

        let r_env2 = ActivationFrame::allocate(3);
        vm.env = r_env2.extends(&vm.env);

        let mut env1 = Environment::allocate();
        env1.names = vec!["x".into(), "y".into(), "z".into()];
        let mut env2 = Environment::allocate();
        env2.names = vec!["a".into(), "b".into(), "c".into()];
        env2.next = Some(env1);

        let expr = lexpr::from_str("y").unwrap().into();
        let mut m = ctx.meaning(expr, env2, true).unwrap();

        m(vm);
        assert_eq!(vm.val, Scm::int(42));
    }

    #[test]
    fn predefined_ref() {
        let mut ctx = MockContext::new();

        let expr = lexpr::from_str("foo").unwrap().into();
        let mut m = ctx.meaning(expr, Environment::allocate(), true).unwrap();

        let vm = &mut VirtualMachine::default();
        vm.globals = vec![Scm::string("I am FOO"), Scm::string("I am BAR")];
        m(vm);
        assert_eq!(vm.val.as_string().unwrap(), "I am FOO");
    }

    #[test]
    fn alternative() {
        let mut ctx = MockContext::new();
        let env = Environment::allocate();

        let vm = &mut VirtualMachine::default();

        let expr = lexpr::from_str("(if #t 1 2)").unwrap().into();
        let mut m = ctx.meaning(expr, env, true).unwrap();
        m(vm);
        assert_eq!(vm.val, Scm::int(1));

        let expr = lexpr::from_str("(if 0 1 2)").unwrap().into();
        let mut m = ctx.meaning(expr, env, true).unwrap();
        m(vm);
        assert_eq!(vm.val, Scm::int(1));

        let expr = lexpr::from_str("(if #f 1 2)").unwrap().into();
        let mut m = ctx.meaning(expr, env, true).unwrap();
        m(vm);
        assert_eq!(vm.val, Scm::int(2));
    }

    #[test]
    fn shallow_set() {
        let mut ctx = MockContext::new();

        let vm = &mut VirtualMachine::default();
        vm.env = ActivationFrame::allocate(3);
        vm.env.set_argument(0, Scm::int(1));
        vm.env.set_argument(1, Scm::int(2));
        vm.env.set_argument(2, Scm::int(3));

        let mut env = Environment::allocate();
        env.names = vec!["x".into(), "y".into(), "z".into()];

        let expr = lexpr::from_str("(set! y 42)").unwrap().into();
        let mut m = ctx.meaning(expr, env, true).unwrap();

        m(vm);
        assert_eq!(*vm.env.argument(1), Scm::int(42));
    }

    #[test]
    fn sequence() {
        let mut ctx = MockContext::new();

        let vm = &mut VirtualMachine::default();

        let expr = lexpr::from_str("(begin 1 2 3)").unwrap().into();
        let mut m = ctx.meaning(expr, Environment::allocate(), true).unwrap();

        m(vm);
        assert_eq!(vm.val, Scm::int(3));
    }

    #[test]
    fn sequence_sideeffects_occur() {
        let mut ctx = MockContext::new();

        let vm = &mut VirtualMachine::default();
        vm.env = ActivationFrame::allocate(3);
        vm.env.set_argument(0, Scm::int(42));
        vm.env.set_argument(1, Scm::int(42));
        vm.env.set_argument(2, Scm::int(42));

        let mut env = Environment::allocate();
        env.names = vec!["x".into(), "y".into(), "z".into()];

        let expr = lexpr::from_str("(begin (set! x 10) (set! y 20) 0)")
            .unwrap()
            .into();
        let mut m = ctx.meaning(expr, env, true).unwrap();

        m(vm);
        assert_eq!(*vm.env.argument(0), Scm::int(10));
        assert_eq!(*vm.env.argument(1), Scm::int(20));
        assert_eq!(*vm.env.argument(2), Scm::int(42));
    }

    #[test]
    fn abstraction() {
        let mut ctx = MockContext::new();

        let expr = lexpr::from_str("(lambda (x) x)").unwrap().into();
        let mut m = ctx.meaning(expr, Environment::allocate(), true).unwrap();

        let vm = &mut VirtualMachine::default();
        m(vm);
        assert!(vm.val.is_primitive());
    }

    #[test]
    fn regular_application() {
        let mut ctx = MockContext::new();
        ctx.g_current.names.push("foo".into());

        let expr = lexpr::from_str("(begin (set! foo (lambda (x y z) y)) (foo 1 2 3))")
            .unwrap()
            .into();
        let mut m = ctx.meaning(expr, Environment::allocate(), true).unwrap();

        let vm = &mut VirtualMachine::default();
        vm.mut_globals.push(Scm::uninitialized());

        m(vm);
        assert_eq!(vm.val, Scm::int(2));
    }

    #[test]
    fn application_mutable_closure() {
        let mut ctx = MockContext::new();
        ctx.g_current.names.push("make_counter".into());
        ctx.g_current.names.push("counter".into());

        let expr = lexpr::from_str("
            (begin
                (set! make_counter
                      (lambda (n)
                         (lambda () (set! n (+ n 1)))))
                (set! counter (make_counter 5))
                (counter)
                (counter)
                (counter)
                )")
            .unwrap()
            .into();
        let mut m = ctx.meaning(expr, Environment::allocate(), true).unwrap();

        let vm = &mut VirtualMachine::default();
        vm.mut_globals.push(Scm::uninitialized());
        vm.mut_globals.push(Scm::uninitialized());

        m(vm);
        assert_eq!(vm.val, Scm::int(2));
    }
}
