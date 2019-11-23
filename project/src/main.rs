mod ast;
mod env;
mod eval;
mod objectify;
mod parsing;
mod sexpr;
mod source;
mod symbol;
mod value;

use crate::ast::{Arity, Ast, FunctionDescription, MagicKeyword, RuntimePrimitive, Variable};
use crate::env::EnvChain;
use crate::objectify::{car, cdr, ObjectifyError, Result, Translate};
use crate::sexpr::TrackedSexpr as Sexpr;
use crate::source::SourceLocation::NoSource;
use crate::value::Value;
use ast::{Alternative, AstNode, Constant, Transformer, Visited};
use env::{Env, EnvAccess, Environment, GlobalRuntimeEnv, LexicalRuntimeEnv};
use lexpr::sexp;
use std::collections::HashMap;

fn main() {
    let ast: AstNode = Alternative::new(
        Constant::new(1, NoSource),
        Constant::new(2, NoSource),
        Constant::new(3, NoSource),
        NoSource,
    );
    println!("{:?}", ast);

    let ast = ast.transform(&mut AllConstZero);
    println!("{:?}", ast);

    let mut predef = Env::new(Environment::Empty);
    predef = predef.extend(Variable::predefined(
        "cons",
        FunctionDescription::new(Arity::Exact(2), "cons a b"),
    ));
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
    println!(
        "{:?}",
        trans.objectify_toplevel(&sexp!(((lambda x x) 1 2)).into())
    );
    println!("{:?}", trans.objectify_toplevel(&sexp!((cons 1 2)).into()));

    let mut runtime_predef = HashMap::new();
    runtime_predef.insert("cons".into(), RuntimePrimitive::new(Arity::Exact(2), cons));

    let mut sr = &mut EnvChain::new();
    let mut sg = &mut GlobalRuntimeEnv::new(runtime_predef);

    println!(
        "{:?}",
        trans
            .objectify_toplevel(&sexp!(42).into())
            .unwrap()
            .eval(sr, sg)
    );

    let ast = trans.objectify_toplevel(&sexp!(foo).into()).unwrap();
    trans.global_env.update_runtime_globals(&mut sg);
    println!("{:?}", ast.eval(sr, sg));

    println!(
        "{:?}",
        trans
            .objectify_toplevel(&sexp!(((lambda (x) x) 42)).into())
            .unwrap()
            .eval(sr, sg)
    );

    println!(
        "{:?}",
        trans
            .objectify_toplevel(&sexp!(((lambda x x) 1 2 3)).into())
            .unwrap()
            .eval(sr, sg)
    );

    println!(
        "{:?}",
        trans
            .objectify_toplevel(&sexp!((lambda (x) x)).into())
            .unwrap()
            .eval(sr, sg)
    );

    //println!("{:?}", trans.objectify_toplevel(&sexp!((begin (#"set!" foo (lambda (x) x)) (foo 10))).into()).unwrap().eval(sr, sg));
}

fn expand_lambda(trans: &mut Translate, expr: &Sexpr, env: &Env) -> Result<AstNode> {
    let def = &cdr(expr)?;
    let names = car(def)?;
    let body = cdr(def)?;
    trans.objectify_function(names, &body, env, expr.source().clone())
}

fn expand_begin(trans: &mut Translate, expr: &Sexpr, env: &Env) -> Result<AstNode> {
    trans.objectify_sequence(&cdr(expr)?, env)
}

fn expand_assign(trans: &mut Translate, expr: &Sexpr, env: &Env) -> Result<AstNode> {
    let parts = expr.as_proper_list().ok_or(ObjectifyError::ExpectedList)?;
    trans.objectify_assignment(&parts[1], &parts[2], env, expr.source().clone())
}

struct AllConstZero;

impl Transformer for AllConstZero {
    fn visit(&mut self, node: &AstNode) -> Visited {
        if let Some(c) = node.downcast_ref::<Constant>() {
            Visited::Transformed(Constant::new(0, c.source().clone()))
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
