pub mod scheme {
    use crate::ast::AstNode;
    use crate::env::Env;
    use crate::objectify::Result;
    use crate::objectify::{car, cdr, Translate};
    use crate::objectify::{ObjectifyError, ObjectifyErrorKind};
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

    pub fn cons(mut args: Vec<Value>) -> Value {
        let cdr = args.pop().unwrap();
        let car = args.pop().unwrap();
        Value::cons(car, cdr)
    }
}
