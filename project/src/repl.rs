use crate::language::scheme::{cons, expand_assign, expand_begin, expand_lambda};
use crate::{
    ast::{Arity, Ast, FunctionDescription, MagicKeyword, RuntimePrimitive, Variable},
    env::{Env, EnvAccess, EnvChain, Environment, GlobalRuntimeEnv},
    objectify::Translate,
    parsing::parse,
    sexpr::TrackedSexpr,
    source::Source,
};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashMap;

pub fn repl() {
    let mut predef = Env::new(Environment::Empty);
    predef = predef.extend(Variable::predefined(
        "cons",
        FunctionDescription::new(Arity::Exact(2), "cons a b"),
    ));
    predef = predef.extend(Variable::Macro(MagicKeyword::new("lambda", expand_lambda)));
    predef = predef.extend(Variable::Macro(MagicKeyword::new("begin", expand_begin)));
    predef = predef.extend(Variable::Macro(MagicKeyword::new("set!", expand_assign)));

    let mut trans = Translate::from_predefined(predef);

    let mut runtime_predef = HashMap::new();
    runtime_predef.insert("cons".into(), RuntimePrimitive::new(Arity::Exact(2), cons));

    let mut sr = &mut EnvChain::new();
    let mut sg = &mut GlobalRuntimeEnv::new(runtime_predef);

    let mut rl = Editor::<()>::new();
    if rl.load_history("repl.hist.txt").is_err() {}

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let src = Source::from(line);
                let sexpr = TrackedSexpr::from_source(src);
                let obj = trans.objectify_toplevel(&sexpr);
                println!(": {:#?}", obj);
                trans.global_env.update_runtime_globals(&mut sg);
                println!("{:?}", obj.unwrap().eval(sr, sg));
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Readline Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("repl.hist.txt").unwrap();
}
