mod ast;
mod env;
mod eval;
mod objectify;
mod sexpr;
mod value;

use crate::ast::{FunctionDescription, MagicKeyword, Variable, Arity, RuntimePrimitive};
use crate::objectify::{car, cdr, Result, Translate};
use crate::value::Value;
use ast::{Alternative, AstNode, Constant, Visited, Transformer};
use env::{Env, EnvAccess, Environment, LexicalRuntimeEnv, GlobalRuntimeEnv};
use lexpr::sexp;
use std::collections::HashMap;
use crate::env::EnvChain;
use crate::sexpr::Sexpr;

fn main() {
    let ast: AstNode = Alternative::new(Constant::new(1), Constant::new(2), Constant::new(3));
    println!("{:?}", ast);

    let ast = ast.transform(&mut AllConstZero);
    println!("{:?}", ast);

    let mut predef = Env::new(Environment::Empty);
    predef = predef.extend(Variable::Predefined("cons", FunctionDescription::new(Arity::Exact(2), "cons a b")));
    predef = predef.extend(Variable::Macro(MagicKeyword::new("lambda", expand_lambda)));
    predef = predef.extend(Variable::Macro(MagicKeyword::new("begin", expand_begin)));
    predef = predef.extend(Variable::Macro(MagicKeyword::new("set!", expand_assign)));

    let mut trans = Translate::from_predefined(predef);

    println!("{:?}", trans.objectify_toplevel(&sexp!(42).into()));
    println!("{:?}", trans.objectify_toplevel(&sexp!((1 2 3 4)).into()));
    println!(
        "{:?}",
        trans.objectify_toplevel(&sexp!(((lambda (x) x) 42)).into())
    );
    println!("{:?}", trans.objectify_toplevel(&sexp!(((lambda x x) 1 2)).into()));
    println!("{:?}", trans.objectify_toplevel(&sexp!((cons 1 2)).into()));

    let mut runtime_predef = HashMap::new();
    runtime_predef.insert("cons", RuntimePrimitive::new(Arity::Exact(2), cons));

    let mut sr = &mut EnvChain::new();
    let mut sg = &mut GlobalRuntimeEnv::new(runtime_predef);

    println!("{:?}", trans.objectify_toplevel(&sexp!(42).into()).unwrap().eval(sr, sg));

    let ast =  trans.objectify_toplevel(&sexp!(foo).into()).unwrap();
    trans.global_env.update_runtime_globals(&mut sg);
    println!("{:?}", ast.eval(sr, sg));

    println!("{:?}", trans.objectify_toplevel(&sexp!(((lambda (x) x) 42)).into()).unwrap().eval(sr, sg));

    println!("{:?}", trans.objectify_toplevel(&sexp!(((lambda x x) 1 2 3)).into()).unwrap().eval(sr, sg));

    println!("{:?}", trans.objectify_toplevel(&sexp!((lambda (x) x)).into()).unwrap().eval(sr, sg));

    println!("{:?}", trans.objectify_toplevel(&sexp!((begin (#"set!" foo (lambda (x) x)) (foo 10))).into()).unwrap().eval(sr, sg));
}

fn expand_lambda(trans: &mut Translate, expr: &Sexpr, env: &Env) -> Result<AstNode> {
    let names = car(cdr(expr)?)?;
    let body = cdr(cdr(expr)?)?;
    trans.objectify_function(names, body, env)
}

fn expand_begin(trans: &mut Translate, expr: &Sexpr, env: &Env) -> Result<AstNode> {
    trans.objectify_sequence(cdr(expr)?, env)
}

fn expand_assign(trans: &mut Translate, expr: &Sexpr, env: &Env) -> Result<AstNode> {
    let var = car(cdr(expr)?)?;
    let val = car(cdr(cdr(expr)?)?)?;
    trans.objectify_assignment(var, val, env)
}

struct AllConstZero;

impl Transformer for AllConstZero {
    fn visit(&mut self, node: &AstNode) -> Visited {
        if let Some(_) = node.downcast_ref::<Constant>() {
            Visited::Transformed(Constant::new(0))
        } else {
            Visited::Identity
        }
    }
}

fn cons(mut args: Vec<Value>) -> Value {
    let cdr = args.pop().unwrap();
    let car = args.pop().unwrap();
    Value::cons(car, cdr)
}