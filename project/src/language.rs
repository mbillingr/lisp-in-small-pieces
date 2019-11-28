pub mod scheme {
    use crate::ast::AstNode;
    use crate::env::Env;
    use crate::objectify::{car, cdr, Translate};
    use crate::objectify::{decons, Result};
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

    pub fn cons(mut args: Vec<Value>) -> Value {
        let cdr = args.pop().unwrap();
        let car = args.pop().unwrap();
        Value::cons(car, cdr)
    }

    pub fn is_eq(args: Vec<Value>) -> Value {
        match &args[..] {
            [a, b] => Value::bool(Value::is_eq(a, b)),
            _ => unreachable!(),
        }
    }

    pub fn multiply(args: Vec<Value>) -> Value {
        match args[..] {
            [Value::Int(a), Value::Int(b)] => Value::Int(a * b),
            [_, _] => panic!("Invalid operand types in multiplication"),
            _ => unreachable!(),
        }
    }

    pub fn divide(args: Vec<Value>) -> Value {
        match args[..] {
            [Value::Int(a), Value::Int(b)] => Value::Int(a / b),
            [_, _] => panic!("Invalid operand types in division"),
            _ => unreachable!(),
        }
    }

    pub fn add(args: Vec<Value>) -> Value {
        match args[..] {
            [Value::Int(a), Value::Int(b)] => Value::Int(a + b),
            [_, _] => panic!("Invalid operand types in addition"),
            _ => unreachable!(),
        }
    }

    pub fn subtract(args: Vec<Value>) -> Value {
        match args[..] {
            [Value::Int(a), Value::Int(b)] => Value::Int(a - b),
            [_, _] => panic!("Invalid operand types in subtraction"),
            _ => unreachable!(),
        }
    }
}
