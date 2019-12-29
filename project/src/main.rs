mod ast;
mod ast_transform;
mod bytecode;
mod env;
mod error;
mod eval;
mod language;
mod objectify;
mod parsing;
mod repl;
mod scm;
mod sexpr;
mod source;
mod symbol;

use crate::ast::{Arity, FunctionDescription, MagicKeyword, RuntimePrimitive, Variable};
use crate::language::scheme::{cons, expand_assign, expand_begin, expand_lambda};
use crate::objectify::Translate;
use crate::source::SourceLocation::NoSource;
use ast::{Alternative, AstNode, Constant, Transformer, Visited};
use env::Env;
use lexpr::sexp;
use repl::repl;
use std::collections::HashMap;

use bdwgc_alloc::Allocator;

#[cfg(not(test))]
#[global_allocator]
static GLOBAL_ALLOCATOR: Allocator = Allocator;

fn main() {
    unsafe { Allocator::initialize() }

    let ast: AstNode = Alternative::new(
        Constant::int(1),
        Constant::int(2),
        Constant::int(3),
        NoSource,
    );
    println!("{:?}", ast);

    let ast = ast.transform(&mut AllConstZero);
    println!("{:?}", ast);

    /*let mut predef = Env::new(Environment::Empty);
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

    let sr = &mut EnvChain::new();
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
    );*/

    //println!("{:?}", trans.objectify_toplevel(&sexp!((begin (#"set!" foo (lambda (x) x)) (foo 10))).into()).unwrap().eval(sr, sg));

    repl()
}

struct AllConstZero;

impl Transformer for AllConstZero {
    fn visit(&mut self, node: &AstNode) -> Visited {
        if let Some(_) = node.downcast_ref::<Constant>() {
            Visited::Transformed(Constant::int(0))
        } else {
            Visited::Identity
        }
    }
}
