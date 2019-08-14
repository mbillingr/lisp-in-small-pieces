#[macro_use]
extern crate gc_derive;

use lexpr;
use rustyline::Editor;
use std::str::FromStr;
use std::rc::Rc;
use gc::Gc;

fn main() {
    let env = Value::cons(Value::cons(Value::Symbol("x"), Value::Int(0)), Value::Nil);
    loop {
        let expr = read();
        let result = evaluate(&expr, &env);
        println!("{:?}", result);
    }
}

#[derive(Clone, Trace, Finalize)]
enum Value {
    Nil,
    Int(i64),
    Symbol(&'static str),
    Pair(Gc<[Value; 2]>),

    Function(Function),
    Closure(Closure),
}

#[derive(Clone, Trace, Finalize)]
struct Function {
    #[unsafe_ignore_trace]
    func: fn(&Value) -> Value
}

#[derive(Clone, Trace, Finalize)]
struct Closure {
    #[unsafe_ignore_trace]
    func: fn(&Value, &[Value]) -> Value,
    vars: Gc<Vec<Value>>,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Value::*;
        match self {
            Nil => write!(f, "'()"),
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

    fn cons(car: Value, cdr: Value) -> Value {
        Value::Pair(Gc::new([car, cdr]))
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
}

impl From<lexpr::Value> for Value {
    fn from(lv: lexpr::Value) -> Self {
        use lexpr::Value::*;
        match lv {
            Nil | Null => Value::Nil,
            Number(n) => if let Some(i) = n.as_i64() { Value::Int(i) } else { unimplemented!() }
            Symbol(s) => Value::Symbol(Box::leak(s)),
            Cons(cons) => {
                let pair = cons.into_pair();
                Value::cons(pair.0.into(), pair.1.into())
            }
            _ => unimplemented!()
        }
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
            Symbol("lambda") => make_function(expr.cdr().car(), expr.cdr().cdr(), env),
            _ => unimplemented!()
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

fn extend(env: &Value, variables: &Value, values: &Value) -> Value {
    unimplemented!()
}

fn eprogn(exps: &Value, env: &Value) -> Value {
    unimplemented!()
}

fn make_function(params: &Value, body: &Value, env: &Value) -> Value {
    Value::Closure(Closure {
        func: |args, closure| {
            let params = &closure[0];
            let body = &closure[1];
            let env = &closure[2];
            eprogn(body, &extend(env, params, args))
        },
        vars: Gc::new(vec![params.clone(), body.clone(), env.clone()]),
    })
}
