use common::{extend, is_atomic, is_self_evaluating, lookup, read, update, Result};
use lisp_core::common::{BasicLispValue, CombinedLispOps, NumericLispValue};
use lisp_core::simple::Value as lcValue;
use std::rc::Rc;

// Unfortunately, this interpreter is not properly tail-recursive.

fn main() {
    let env = init_global_environment();

    loop {
        match read().and_then(|expr| evaluate(&expr, &env)) {
            Ok(value) => println!("{:?}", value),
            Err(e) => eprintln!("Error: {:?}", e),
        }
    }
}

macro_rules! def_primitive {
    ($env:expr, $name:expr, $func:expr) => {
        define!($env, $name, lcValue::Function(Callable(Rc::new($func))))
    };
}

macro_rules! define {
    ($env:expr, $name:expr, $value:expr) => {
        Value::cons(Value::cons(lcValue::Symbol($name), $value), $env)
    };
}

fn init_global_environment() -> Value {
    let env = define!(lcValue::Nil, "x", lcValue::Undefined);
    let env = define!(env, "y", lcValue::Undefined);
    let env = define!(env, "z", lcValue::Undefined);
    let env = define!(env, "fib", lcValue::Undefined);
    let env = define!(env, "list", lcValue::Undefined);
    let env = def_primitive!(env, "cons", |args: &Value| Ok(Value::cons(
        args.car()?.clone(),
        args.cadr()?.clone()
    )));
    let env = def_primitive!(env, "car", |args: &Value| Ok(args.caar()?.clone()));
    let env = def_primitive!(env, "cdr", |args: &Value| Ok(args.cdar()?.clone()));
    let env = def_primitive!(env, "set-car!", |args: &Value| {
        args.car()?.set_car(args.cadr()?.clone())?;
        Ok(lcValue::Undefined)
    });
    let env = def_primitive!(env, "set-cdr!", |args: &Value| {
        args.car()?.set_cdr(args.cadr()?.clone())?;
        Ok(lcValue::Undefined)
    });
    let env = def_primitive!(env, "+", |args: &Value| Ok(args
        .car()?
        .add(args.cadr()?)?));
    let env = def_primitive!(env, "-", |args: &Value| Ok(args
        .car()?
        .sub(args.cadr()?)?));
    let env = def_primitive!(env, "<", |args: &Value| Ok(args
        .car()?
        .is_less(args.cadr()?)?));
    env
}

fn evaluate(expr: &Value, env: &Value) -> Result<Value> {
    use lcValue::*;
    if is_atomic(expr) {
        if expr.is_symbol() {
            Ok(lookup(expr, env)?.clone())
        } else if is_self_evaluating(expr) {
            Ok(expr.clone())
        } else {
            panic!("Cannot evaluate {:?}", expr)
        }
    } else {
        match expr.car()? {
            Symbol("quote") => Ok(expr.cadr()?.clone()),
            Symbol("if") => {
                if evaluate(expr.cadr()?, env)?.as_bool().unwrap() {
                    evaluate(expr.caddr()?, env)
                } else {
                    evaluate(expr.cadddr()?, env)
                }
            }
            Symbol("begin") => eprogn(expr.cdr()?, env),
            Symbol("set!") => update(expr.cadr()?, env, &evaluate(expr.caddr()?, env)?),
            Symbol("lambda") => make_function(expr.cadr()?, expr.cddr()?, env),
            _ => invoke(&evaluate(expr.car()?, env)?, &evlis(expr.cdr()?, env)?),
        }
    }
}

fn eprogn(exps: &Value, env: &Value) -> Result<Value> {
    if exps.is_pair() {
        let rest = exps.cdr()?;
        if rest.is_pair() {
            evaluate(exps.car()?, env)?;
            eprogn(exps.cdr()?, env)
        } else {
            evaluate(exps.car()?, env)
        }
    } else {
        Ok(lcValue::Undefined)
    }
}

fn evlis(exps: &Value, env: &Value) -> Result<Value> {
    if exps.is_pair() {
        let argument = evaluate(exps.car()?, env)?;
        Ok(Value::cons(argument, evlis(exps.cdr()?, env)?))
    } else {
        Ok(lcValue::Nil)
    }
}

fn make_function(params: &Value, body: &Value, env: &Value) -> Result<Value> {
    let params = params.clone();
    let body = body.clone();
    let env = env.clone();
    Ok(lcValue::Function(Callable(Rc::new(move |args| {
        eprogn(&body, &extend(&env, &params, args)?)
    }))))
}

fn invoke(func: &Value, args: &Value) -> Result<Value> {
    match func {
        lcValue::Function(Callable(func)) => func(args),
        _ => panic!("Not a function: {:?}", func),
    }
}

type Value = lcValue<Callable>;

#[derive(Clone)]
struct Callable(Rc<dyn Fn(&Value) -> Result<Value>>);

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<callable {:p}>", &*self.0)
    }
}
