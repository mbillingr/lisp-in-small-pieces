use lisp_core::managed::{Context, ErrorKind, LispData, LispError, LispOps, LispValue};

//static QUOTE: LispValue = LispValue::Symbol(&"quote");
static BEGIN: &&str = &"begin";
static IF: &&str = &"if";
static LAMBDA: &&str = &"lambda";
static QUOTE: &&str = &"quote";
static SET: &&str = &"set!";

fn main() {
    println!(
        "Welcome to the Rust implementation of the basic Scheme
interpreter from the book Lisp In Small Pieces, chapter 1.
Built {}
commit {}
Enjoy.
",
        env!("BUILD_DATE"),
        env!("GIT_HASH")
    );

    let mut context = Context::new();
    context.register_symbol(LispValue::Symbol(QUOTE));
    context.register_symbol(LispValue::Symbol(LAMBDA));

    let mut env = LispValue::Nil;
    env = extend(
        env,
        LispValue::symbol("x"),
        LispValue::Undefined,
        &mut context,
    )
    .unwrap();
    env = extend(
        env,
        LispValue::symbol("y"),
        LispValue::Undefined,
        &mut context,
    )
    .unwrap();
    env = extend(
        env,
        LispValue::symbol("fib"),
        LispValue::Undefined,
        &mut context,
    )
    .unwrap();
    env = extend(
        env,
        LispValue::symbol("list"),
        LispValue::Undefined,
        &mut context,
    )
    .unwrap();
    env = extend(
        env,
        LispValue::symbol("cons"),
        LispValue::Function(|args, ctx| {
            let car = args.car().unwrap();
            let cdr = args.cadr().unwrap();
            ctx.cons(car, cdr)
        }),
        &mut context,
    )
    .unwrap();
    env = extend(
        env,
        LispValue::symbol("+"),
        LispValue::Function(|args, _ctx| {
            let a = args.car().unwrap();
            let b = args.cadr().unwrap();
            match (a, b) {
                (LispValue::Integer(a), LispValue::Integer(b)) => LispValue::Integer(a + b),
                _ => panic!("Type Error"),
            }
        }),
        &mut context,
    )
    .unwrap();
    env = extend(
        env,
        LispValue::symbol("-"),
        LispValue::Function(|args, _ctx| {
            let a = args.car().unwrap();
            let b = args.cadr().unwrap();
            match (a, b) {
                (LispValue::Integer(a), LispValue::Integer(b)) => LispValue::Integer(a - b),
                _ => panic!("Type Error"),
            }
        }),
        &mut context,
    )
    .unwrap();
    env = extend(
        env,
        LispValue::symbol("<"),
        LispValue::Function(|args, _ctx| {
            let a = args.car().unwrap();
            let b = args.cadr().unwrap();
            match (a, b) {
                (LispValue::Integer(a), LispValue::Integer(b)) => LispValue::bool(a < b),
                _ => panic!("Type Error"),
            }
        }),
        &mut context,
    )
    .unwrap();
    context.push(env);

    loop {
        let expr = match context.read() {
            Ok(x) => x,
            Err(e) => match *e.kind {
                ErrorKind::Eof => break,
                _ => {
                    eprintln!("Error: {:?}", e);
                    continue;
                }
            },
        };

        let env = context.pop().unwrap();
        context.push(env);

        match evaluate(expr, env, &mut context) {
            Ok(value) => println!("{}", value),
            Err(e) => eprintln!("{:?}", e),
        }
    }
}

fn evaluate(
    expr: LispValue,
    env: LispValue,
    context: &mut Context<LispValue>,
) -> Result<LispValue> {
    if is_atom(expr) {
        if expr.is_symbol() {
            lookup(expr, env)
        } else if expr.is_number() || expr.is_bool() {
            Ok(expr)
        } else {
            unimplemented!()
        }
    } else {
        match expr.car().unwrap() {
            LispValue::Symbol(x) if x == QUOTE => Ok(expr.cadr()?),
            LispValue::Symbol(x) if x == IF => {
                context.push(env);
                context.push(expr);
                if evaluate(expr.cadr()?, env, context)?.is_true() {
                    let expr = context.pop().unwrap();
                    let env = context.pop().unwrap();
                    evaluate(expr.caddr()?, env, context)
                } else {
                    let expr = context.pop().unwrap();
                    let env = context.pop().unwrap();
                    evaluate(expr.cadddr()?, env, context)
                }
            }
            LispValue::Symbol(x) if x == BEGIN => eprogn(expr.cdr()?, env, context),
            LispValue::Symbol(x) if x == SET => {
                let var = expr.cadr()?;
                context.push(env);
                context.push(var);
                let val = evaluate(expr.caddr()?, env, context)?;
                let var = context.pop().unwrap();
                let env = context.pop().unwrap();
                update(var, env, val, context)
            }
            LispValue::Symbol(x) if x == LAMBDA => {
                make_function(expr.cadr()?, expr.cddr()?, env, context)
            }
            _ => {
                context.push(env);
                context.push(expr);
                let func = evaluate(expr.car()?, env, context)?;
                let expr = context.pop().unwrap();
                let env = context.pop().unwrap();
                context.push(func);
                let args = evlis(expr.cdr()?, env, context)?;
                let func = context.pop().unwrap();
                invoke(func, args, context)
            }
        }
    }
}

fn is_atom(expr: LispValue) -> bool {
    !expr.is_pair()
}

fn eprogn(exprs: LispValue, env: LispValue, ctx: &mut Context<LispValue>) -> Result<LispValue> {
    if exprs.is_pair() {
        let rest = exprs.cdr().unwrap();
        if rest.is_pair() {
            ctx.push(env);
            ctx.push(rest);
            evaluate(exprs.car().unwrap(), env, ctx).unwrap();
            let rest = ctx.pop().unwrap();
            let env = ctx.pop().unwrap();
            eprogn(rest, env, ctx)
        } else {
            evaluate(exprs.car().unwrap(), env, ctx)
        }
    } else {
        Ok(LispValue::Undefined)
    }
}

fn evlis(exprs: LispValue, env: LispValue, ctx: &mut Context<LispValue>) -> Result<LispValue> {
    if exprs.is_pair() {
        ctx.push(env);
        ctx.push(exprs);
        let arg = evaluate(exprs.car()?, env, ctx)?;
        let exprs = ctx.pop().unwrap();
        let env = ctx.pop().unwrap();
        ctx.push(arg);
        let rest_args = evlis(exprs.cdr()?, env, ctx)?;
        let arg = ctx.pop().unwrap();
        Ok(ctx.cons(arg, rest_args))
    } else {
        Ok(LispValue::Nil)
    }
}

fn lookup(id: LispValue, env: LispValue) -> Result<LispValue> {
    if env.is_pair() {
        if env.caar().unwrap() == id {
            Ok(env.cdar()?)
        } else {
            lookup(id, env.cdr().unwrap())
        }
    } else {
        InterpreterError::UnboundSymbol(id).into()
    }
}

fn update(
    id: LispValue,
    env: LispValue,
    value: LispValue,
    ctx: &mut Context<LispValue>,
) -> Result<LispValue> {
    if env.is_pair() {
        if env.caar().unwrap() == id {
            env.car().unwrap().set_cdr(value).unwrap();
            Ok(value)
        } else {
            update(id, env.cdr().unwrap(), value, ctx)
        }
    } else {
        InterpreterError::UnboundSymbol(id).into()
    }
}

fn make_function(
    params: LispValue,
    body: LispValue,
    env: LispValue,
    ctx: &mut Context<LispValue>,
) -> Result<LispValue> {
    ctx.push(LispValue::Symbol(LAMBDA));
    ctx.push(params);
    ctx.push(body);
    ctx.push(env);
    Ok(ctx.record(4))
}

fn extend(
    env: LispValue,
    variables: LispValue,
    values: LispValue,
    ctx: &mut Context<LispValue>,
) -> Result<LispValue> {
    if variables.is_pair() {
        if values.is_pair() {
            let var = variables.car().unwrap();
            let val = values.car().unwrap();
            ctx.push(env);
            ctx.push(variables);
            ctx.push(values);
            let entry = ctx.cons(var, val);
            let values = ctx.pop().unwrap();
            let variables = ctx.pop().unwrap();
            let env = ctx.pop().unwrap();
            ctx.push(entry);
            let rest_extended = extend(env, variables.cdr().unwrap(), values.cdr().unwrap(), ctx)?;
            let entry = ctx.pop().unwrap();
            Ok(ctx.cons(entry, rest_extended))
        } else {
            InterpreterError::TooFewArguments.into()
        }
    } else if variables.is_nil() {
        if values.is_nil() {
            Ok(env)
        } else {
            InterpreterError::TooManyArguments.into()
        }
    } else if variables.is_symbol() {
        ctx.push(env);
        let entry = ctx.cons(variables, values);
        let env = ctx.pop().unwrap();
        Ok(ctx.cons(entry, env))
    } else {
        unreachable!()
    }
}

fn invoke(func: LispValue, args: LispValue, ctx: &mut Context<LispValue>) -> Result<LispValue> {
    match func {
        LispValue::Record(_, 4) => {
            if func.get_array_item(0).unwrap() != LispValue::Symbol(LAMBDA) {
                return InterpreterError::NoFunction.into();
            }
            let params = func.get_array_item(1).unwrap();
            let body = func.get_array_item(2).unwrap();
            let env = func.get_array_item(3).unwrap();
            ctx.push(body);
            let local_env = extend(env, params, args, ctx)?;
            let body = ctx.pop().unwrap();
            eprogn(body, local_env, ctx)
        }
        LispValue::Function(fptr) => Ok(fptr(args, ctx)),
        _ => InterpreterError::NoFunction.into(),
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
struct Error {
    kind: Box<InterpreterError>,
}

#[derive(Debug)]
enum InterpreterError {
    NoFunction,
    TooManyArguments,
    TooFewArguments,
    UnboundSymbol(LispValue),
    LispError(LispError),
}

impl<T> Into<Result<T>> for InterpreterError {
    fn into(self) -> Result<T> {
        panic!("{:?}", self);
        //Err(Error { kind: Box::new(self) })
    }
}

impl<T: Into<InterpreterError>> From<T> for Error {
    fn from(e: T) -> Self {
        Error {
            kind: Box::new(e.into()),
        }
    }
}

impl From<LispError> for InterpreterError {
    fn from(le: LispError) -> Self {
        InterpreterError::LispError(le)
    }
}
