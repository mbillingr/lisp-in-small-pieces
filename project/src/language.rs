pub mod scheme {
    use crate::ast::AstNode;
    use crate::ast::{Arity, FunctionDescription, MagicKeyword, RuntimePrimitive};
    use crate::env::Env;
    use crate::objectify::{car, cdr, Translate};
    use crate::objectify::{decons, Result};
    use crate::objectify::{ObjectifyError, ObjectifyErrorKind};
    use crate::scm::Scm;
    use crate::sexpr::TrackedSexpr;
    use crate::value::Value;
    use crate::Variable;

    macro_rules! predef {
        () => {
            (Env::new(), vec![])
        };

        (primitive $name:expr, =$arity:expr, $func:ident, $($rest:tt)*) => {{
            let (mut env, mut runtime_env) = predef!{$($rest)*};

            env.predef.extend(Variable::predefined(
                $name,
                FunctionDescription::new(Arity::Exact(2), $name),
            ));

            runtime_env.push(Scm::Primitive(RuntimePrimitive::new(Arity::Exact($arity), $func)));

            (env, runtime_env)
        }};

        (macro $name:expr, $func:ident, $($rest:tt)*) => {{
            let (mut env, mut runtime_env) = predef!{$($rest)*};
            env.predef.extend(Variable::Macro(MagicKeyword::new($name, $func)));
            runtime_env.push(Scm::Undefined);
            (env, runtime_env)
        }};
    }

    pub fn init_env() -> (Env, Vec<Scm>) {
        predef! {
            primitive "cons", =2, cons,
            primitive "eq?", =2, is_eq,
            primitive "<", =2, is_less,
            primitive "*", =2, multiply,
            primitive "/", =2, divide,
            primitive "+", =2, add,
            primitive "-", =2, subtract,

            macro "lambda", expand_lambda,
            macro "set!", expand_assign,
            macro "if", expand_alternative,
            macro "quote", expand_quote,
        }
    }

    pub fn expand_lambda(trans: &mut Translate, expr: &TrackedSexpr, env: &Env) -> Result<AstNode> {
        let def = &cdr(expr)?;
        let names = car(def)?;
        let body = cdr(def)?;
        trans.objectify_function(names, &body, env, expr.source().clone())
    }

    pub fn expand_begin(trans: &mut Translate, expr: &TrackedSexpr, env: &Env) -> Result<AstNode> {
        trans.objectify_sequence(&cdr(expr)?, env)
    }

    pub fn expand_assign(trans: &mut Translate, expr: &TrackedSexpr, env: &Env) -> Result<AstNode> {
        let parts = expr.as_proper_list().ok_or(ObjectifyError {
            kind: ObjectifyErrorKind::ExpectedList,
            location: expr.source().clone(),
        })?;
        trans.objectify_assignment(&parts[1], &parts[2], env, expr.source().clone())
    }

    pub fn expand_quote(trans: &mut Translate, expr: &TrackedSexpr, env: &Env) -> Result<AstNode> {
        let body = &cdr(expr)?;
        trans.objectify_quotation(car(body)?, env)
    }

    pub fn expand_alternative(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> Result<AstNode> {
        let rest = cdr(expr)?;
        let (cond, rest) = decons(&rest)?;
        let (yes, rest) = decons(&rest)?;
        let (no, _) = decons(&rest)?;
        trans.objectify_alternative(cond, yes, no, env, expr.source().clone())
    }

    pub fn cons(mut args: &[Scm]) -> Scm {
        let car = args[0];
        let cdr = args[1];
        Scm::cons(car, cdr)
    }

    pub fn is_eq(args: &[Scm]) -> Scm {
        match &args[..] {
            [a, b] => Scm::bool(Scm::ptr_eq(a, b)),
            _ => unreachable!(),
        }
    }

    pub fn is_less(args: &[Scm]) -> Scm {
        match &args[..] {
            [a, b] => Scm::bool(Scm::num_less(a, b).unwrap()),
            _ => unreachable!(),
        }
    }

    pub fn multiply(args: &[Scm]) -> Scm {
        match args[..] {
            [a, b] => (a * b).unwrap(),
            _ => unreachable!(),
        }
    }

    pub fn divide(args: &[Scm]) -> Scm {
        match args[..] {
            [a, b] => (a / b).unwrap(),
            _ => unreachable!(),
        }
    }

    pub fn add(args: &[Scm]) -> Scm {
        match args[..] {
            [a, b] => (a + b).unwrap(),
            _ => unreachable!(),
        }
    }

    pub fn subtract(args: &[Scm]) -> Scm {
        match args[..] {
            [a, b] => (a - b).unwrap(),
            _ => unreachable!(),
        }
    }
}
