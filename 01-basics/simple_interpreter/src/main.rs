use common::{extend, is_atomic, is_self_evaluating, lookup, read, update, Result};
use lisp_core::common::{BasicLispValue, CombinedLispOps, NumericLispValue};
use lisp_core::simple::Value as lcValue;
use std::ops::Bound;
use std::rc::Rc;

// Unfortunately, this interpreter is not properly tail-recursive.

fn main() {
    let env = init_global_environment();

    let mut c = main_loop(Value::Nil, env).unwrap();

    loop {
        c = c.0().unwrap();
    }
}

fn main_loop(value: Value, env: Value) -> Result<BoundContinuation> {
    println!("{:?}", value);
    evaluate(
        &read()?,
        env.clone(),
        (|value| main_loop(value, env)).into(),
    )
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
    let env = def_primitive!(env, "cons", |args: Value, cont| {
        Ok((move || cont.0(Value::cons(args.car()?.clone(), args.cadr()?.clone()))).into())
    });
    let env = def_primitive!(env, "car", |args: Value, cont| {
        Ok((move || cont.0(args.caar()?.clone())).into())
    });
    let env = def_primitive!(env, "cdr", |args: Value, cont| {
        Ok((move || cont.0(args.cdar()?.clone())).into())
    });
    let env = def_primitive!(env, "+", |args: Value, cont| {
        Ok((move || cont.0(args.car()?.add(args.cadr()?)?)).into())
    });
    let env = def_primitive!(env, "-", |args: Value, cont| {
        Ok((move || cont.0(args.car()?.sub(args.cadr()?)?)).into())
    });
    let env = def_primitive!(env, "<", |args: Value, cont| {
        Ok((move || cont.0(args.car()?.is_less(args.cadr()?)?)).into())
    });
    /*let env = def_primitive!(env, "set-car!", |args: &Value| {
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
        .is_less(args.cadr()?)?));*/
    env
}

fn evaluate(expr: &Value, env: Value, cont: Continuation) -> Result<BoundContinuation> {
    use lcValue::*;
    if is_atomic(expr) {
        if expr.is_symbol() {
            let expr = expr.clone();
            Ok((move || cont.0(lookup(&expr, &env)?.clone())).into())
        } else if is_self_evaluating(expr) {
            let expr = expr.clone();
            Ok((|| cont.0(expr)).into())
        } else {
            panic!("Cannot evaluate {:?}", expr)
        }
    } else {
        match expr.car()? {
            Symbol("quote") => {
                let q = expr.cadr()?.clone();
                Ok((|| cont.0(q)).into())
            }
            Symbol("if") => {
                let expr = expr.clone();
                evaluate(
                    &expr.cadr()?.clone(),
                    env.clone(),
                    (move |cond: Value| {
                        if cond.as_bool().unwrap() {
                            evaluate(expr.caddr()?, env, cont)
                        } else {
                            evaluate(expr.cadddr()?, env, cont)
                        }
                    })
                    .into(),
                )
            }
            Symbol("begin") => eprogn(expr.cdr()?, env, cont),
            Symbol("set!") => {
                let expr = expr.clone();
                evaluate(
                    expr.clone().caddr()?,
                    env.clone(),
                    (move |value| {
                        update(expr.cadr()?, &env, &value)?;
                        Ok((|| cont.0(value)).into())
                    })
                    .into(),
                )
            }
            //update(expr.cadr()?, env, &evaluate(expr.caddr()?, env)?),
            Symbol("lambda") => {
                let expr = expr.clone();
                Ok((move || cont.0(make_function(expr.cadr()?, expr.cddr()?, env)?)).into())
            }
            _ => {
                let expr = expr.clone();
                evaluate(
                    expr.clone().car()?,
                    env.clone(),
                    (move |func| {
                        evlis(
                            expr.cdr()?,
                            env,
                            (move |args| invoke(&func, &args, cont)).into(),
                        )
                    })
                    .into(),
                )
            }
        }
    }
}

fn eprogn(exps: &Value, env: Value, cont: Continuation) -> Result<BoundContinuation> {
    if exps.is_pair() {
        let rest = exps.cdr()?;
        if rest.is_pair() {
            let exps = exps.clone();
            evaluate(
                exps.clone().car()?,
                env.clone(),
                (move |_| eprogn(exps.cdr()?, env, cont)).into(),
            )
        } else {
            evaluate(exps.car()?, env, cont)
        }
    } else {
        Ok((|| cont.0(lcValue::Undefined)).into())
    }
}

fn evlis(exps: &Value, env: Value, cont: Continuation) -> Result<BoundContinuation> {
    if exps.is_pair() {
        let exps = exps.clone();
        evaluate(
            exps.clone().car()?,
            env.clone(),
            (move |arg| {
                evlis(
                    exps.cdr()?,
                    env,
                    (|rest| Ok((|| cont.0(Value::cons(arg, rest))).into())).into(),
                )
            })
            .into(),
        )
    } else {
        Ok((|| cont.0(lcValue::Nil)).into())
    }
}

fn make_function(params: &Value, body: &Value, env: Value) -> Result<Value> {
    let params = params.clone();
    let body = body.clone();
    let env = env.clone();
    Ok(lcValue::Function(Callable(Rc::new(move |args, cont| {
        eprogn(&body, extend(&env, &params, &args)?, cont)
    }))))
}

fn invoke(func: &Value, args: &Value, cont: Continuation) -> Result<BoundContinuation> {
    match func {
        lcValue::Function(Callable(func)) => func(args.clone(), cont),
        _ => panic!("Not a function: {:?}", func),
    }
}

type Value = lcValue<Callable>;

#[derive(Clone)]
struct Callable(Rc<dyn Fn(Value, Continuation) -> Result<BoundContinuation>>);

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<callable {:p}>", &*self.0)
    }
}

struct Continuation(Box<dyn FnOnce(Value) -> Result<BoundContinuation>>);
struct BoundContinuation(Box<dyn FnOnce() -> Result<BoundContinuation>>);

impl<T: FnOnce(Value) -> Result<BoundContinuation> + 'static> From<T> for Continuation {
    fn from(f: T) -> Self {
        Continuation(Box::new(f))
    }
}

impl<T: FnOnce() -> Result<BoundContinuation> + 'static> From<T> for BoundContinuation {
    fn from(f: T) -> Self {
        BoundContinuation(Box::new(f))
    }
}
