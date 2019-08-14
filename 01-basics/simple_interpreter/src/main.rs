use lexpr;
use rustyline::Editor;
use std::rc::Rc;
use std::str::FromStr;

macro_rules! def_primitive {
    ($env:expr, $name:expr, $func:expr) => {
        define!($env, $name, Value::Function(Function { func: $func }))
    };
}

macro_rules! define {
    ($env:expr, $name:expr, $value:expr) => {
        Value::cons(Value::cons(Value::Symbol($name), $value), $env)
    };
}

fn main() {
    let env = define!(Value::Nil, "x", Value::Undefined);
    let env = define!(env, "y", Value::Undefined);
    let env = define!(env, "z", Value::Undefined);
    let env = define!(env, "fib", Value::Undefined);
    let env = define!(env, "list", Value::Undefined);
    let env = def_primitive!(env, "cons", |args: &Value| Value::cons(
        args.car().clone(),
        args.cdr().car().clone()
    ));
    let env = def_primitive!(env, "car", |args: &Value| args.car().car().clone());
    let env = def_primitive!(env, "cdr", |args: &Value| args.car().cdr().clone());
    let env = def_primitive!(env, "set-car!", |args: &Value| {
        args.car().set_car(args.cdr().car().clone());
        Value::Undefined
    });
    let env = def_primitive!(env, "set-cdr!", |args: &Value| {
        args.car().set_cdr(args.cdr().car().clone());
        Value::Undefined
    });
    let env = def_primitive!(env, "+", |args: &Value| args.car().add(args.cdr().car()));
    let env = def_primitive!(env, "-", |args: &Value| args.car().sub(args.cdr().car()));
    let env = def_primitive!(env, "<", |args: &Value| args
        .car()
        .is_less(args.cdr().car()));

    loop {
        let expr = read();
        let result = evaluate(&expr, &env);
        println!("{:?}", result);
    }
}

fn read() -> Value {
    let mut rl = Editor::<()>::new();
    match rl.readline(">> ") {
        Ok(line) => lexpr::Value::from_str(&line).unwrap().into(),
        Err(e) => panic!("{:?}", e),
    }
}

fn evaluate(expr: &Value, env: &Value) -> Value {
    use Value::*;
    if expr.is_atomic() {
        if expr.is_symbol() {
            lookup(expr, env).clone()
        } else {
            expr.clone()
        }
    } else {
        match expr.car() {
            Symbol("quote") => expr.cdr().car().clone(),
            Symbol("if") => {
                if evaluate(expr.cdr().car(), env).as_bool() {
                    evaluate(expr.cdr().cdr().car(), env)
                } else {
                    evaluate(expr.cdr().cdr().cdr().car(), env)
                }
            }
            Symbol("begin") => eprogn(expr.cdr(), env),
            Symbol("set!") => update(
                expr.cdr().car(),
                env,
                &evaluate(expr.cdr().cdr().car(), env),
            ),
            Symbol("lambda") => make_function(expr.cdr().car(), expr.cdr().cdr(), env),
            _ => invoke(&evaluate(expr.car(), env), &evlis(expr.cdr(), env)),
        }
    }
}

fn lookup<'a>(id: &Value, env: &'a Value) -> &'a Value {
    if env.is_null() {
        panic!("Unbound Variable {:?}", id)
    }
    if env.car().car() == id {
        env.car().cdr()
    } else {
        lookup(id, env.cdr())
    }
}

fn update(id: &Value, env: &Value, value: &Value) -> Value {
    if env.is_null() {
        panic!("Unbound Variable {:?}", id)
    }
    if env.car().car() == id {
        env.car().set_cdr(value.clone());
        value.clone()
    } else {
        update(id, env.cdr(), value)
    }
}

fn extend(env: &Value, variables: &Value, values: &Value) -> Value {
    if variables.is_pair() {
        if values.is_pair() {
            let entry = Value::cons(variables.car().clone(), values.car().clone());
            Value::cons(entry, extend(env, variables.cdr(), values.cdr()))
        } else {
            panic!("Too few values")
        }
    } else if variables.is_null() {
        if values.is_null() {
            env.clone()
        } else {
            panic!("Too many values")
        }
    } else if variables.is_symbol() {
        Value::cons(Value::cons(variables.clone(), values.clone()), env.clone())
    } else {
        unreachable!()
    }
}

fn eprogn(exps: &Value, env: &Value) -> Value {
    if exps.is_pair() {
        let rest = exps.cdr();
        if rest.is_pair() {
            evaluate(exps.car(), env);
            eprogn(exps.cdr(), env)
        } else {
            evaluate(exps.car(), env)
        }
    } else {
        Value::Undefined
    }
}

fn evlis(exps: &Value, env: &Value) -> Value {
    if exps.is_pair() {
        let argument = evaluate(exps.car(), env);
        Value::cons(argument, evlis(exps.cdr(), env))
    } else {
        Value::Nil
    }
}

fn make_function(params: &Value, body: &Value, env: &Value) -> Value {
    Value::Closure(Closure {
        func: |args, closure| {
            let params = &closure[0];
            let body = &closure[1];
            let env = &closure[2];
            eprogn(body, &extend(env, params, args))
        },
        vars: Rc::new(vec![params.clone(), body.clone(), env.clone()]),
    })
}

fn invoke(func: &Value, args: &Value) -> Value {
    match func {
        Value::Closure(cls) => (cls.func)(args, &cls.vars),
        Value::Function(func) => (func.func)(args),
        _ => panic!("Not a function: {:?}", func),
    }
}

#[derive(Clone)]
enum Value {
    Undefined,
    Nil,
    True,
    False,
    Int(i64),
    Symbol(&'static str),
    Pair(Rc<[Value; 2]>),

    Function(Function),
    Closure(Closure),
}

#[derive(Clone)]
struct Function {
    func: fn(&Value) -> Value,
}

#[derive(Clone)]
struct Closure {
    func: fn(&Value, &[Value]) -> Value,
    vars: Rc<Vec<Value>>,
}

impl From<fn(&Value) -> Value> for Value {
    fn from(func: fn(&Value) -> Value) -> Value {
        Value::Function(Function { func })
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Value::*;
        match self {
            Undefined => write!(f, "#UNDEFINED"),
            Nil => write!(f, "'()"),
            True => write!(f, "#t"),
            False => write!(f, "#f"),
            Int(i) => write!(f, "{}", i),
            Symbol(s) => write!(f, "{}", s),
            Pair(p) => write!(f, "{:?}", p),
            Function(func) => write!(f, "func<{:p}>", func),
            Closure(c) => write!(f, "closure<{:p}>", c),
        }
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        use Value::*;
        match (self, rhs) {
            (Nil, Nil) => true,
            (Int(a), Int(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Pair(a), Pair(b)) => a.as_ptr() == b.as_ptr(),
            _ => false,
        }
    }
}

impl Value {
    fn bool(b: bool) -> Self {
        match b {
            true => Value::True,
            false => Value::False,
        }
    }

    fn is_null(&self) -> bool {
        match self {
            Value::Nil => true,
            _ => false,
        }
    }

    fn is_atomic(&self) -> bool {
        match self {
            Value::Pair(_) => false,
            _ => true,
        }
    }

    fn is_symbol(&self) -> bool {
        match self {
            Value::Symbol(_) => true,
            _ => false,
        }
    }

    fn is_pair(&self) -> bool {
        match self {
            Value::Pair(_) => true,
            _ => false,
        }
    }

    fn as_bool(&self) -> bool {
        match self {
            Value::True => true,
            Value::False => false,
            _ => panic!("Not a bool"),
        }
    }

    fn cons(car: Value, cdr: Value) -> Value {
        Value::Pair(Rc::new([car, cdr]))
    }

    fn car(&self) -> &Value {
        match self {
            Value::Pair(p) => &p[0],
            _ => panic!("Not a pair"),
        }
    }

    fn cdr(&self) -> &Value {
        match self {
            Value::Pair(p) => &p[1],
            _ => panic!("Not a pair"),
        }
    }

    fn set_car(&self, value: Value) {
        match self {
            Value::Pair(p) => {
                unsafe {
                    *(&p[0] as *const _ as *mut _) = value
                }
            }
            _ => panic!("Not a pair"),
        }
    }

    fn set_cdr(&self, value: Value) {
        match self {
            Value::Pair(p) => {
                unsafe {
                    *(&p[1] as *const _ as *mut _) = value
                }
            }
            _ => panic!("Not a pair"),
        }
    }

    fn add(&self, rhs: &Self) -> Value {
        use Value::*;
        match (self, rhs) {
            (Int(a), Int(b)) => Int(a + b),
            _ => panic!("Not a number"),
        }
    }

    fn sub(&self, rhs: &Self) -> Value {
        use Value::*;
        match (self, rhs) {
            (Int(a), Int(b)) => Int(a - b),
            _ => panic!("Not a number"),
        }
    }

    fn is_less(&self, rhs: &Self) -> Value {
        use Value::*;
        match (self, rhs) {
            (Int(a), Int(b)) => Value::bool(a < b),
            _ => panic!("Not a number"),
        }
    }
}

impl From<lexpr::Value> for Value {
    fn from(lv: lexpr::Value) -> Self {
        use lexpr::Value::*;
        match lv {
            Nil | Null => Value::Nil,
            Bool(true) => Value::True,
            Bool(false) => Value::False,
            Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::Int(i)
                } else {
                    unimplemented!()
                }
            }
            Symbol(s) => Value::Symbol(Box::leak(s)),
            Cons(cons) => {
                let pair = cons.into_pair();
                Value::cons(pair.0.into(), pair.1.into())
            }
            _ => unimplemented!(),
        }
    }
}
