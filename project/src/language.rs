pub mod scheme {
    use crate::ast::AstNode;
    use crate::env::Env;
    use crate::objectify::{car, cdr, Translate};
    use crate::objectify::{decons, Result};
    use crate::objectify::{ObjectifyError, ObjectifyErrorKind};
    use crate::scm::Scm;
    use crate::sexpr::TrackedSexpr;
    use crate::value::Value;

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
