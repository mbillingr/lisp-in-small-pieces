use crate::ast_transform::boxify::Boxify;
use crate::ast_transform::flatten_closures::Flatten;
use crate::ast_transform::generate_bytecode::BytecodeGenerator;
use crate::bytecode::{Closure, VirtualMachine};
use crate::{
    ast::{Arity, FunctionDescription, MagicKeyword, RuntimePrimitive, Variable},
    env::{Env, EnvAccess, EnvChain, GlobalRuntimeEnv},
    error::{Error, ErrorContext},
    language::scheme::{
        add, cons, divide, expand_alternative, expand_assign, expand_begin, expand_lambda,
        expand_quote, init_env, is_eq, is_less, multiply, subtract,
    },
    objectify::Translate,
    scm::Scm,
    sexpr::TrackedSexpr,
    source::{Source, SourceLocation},
};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashMap;

pub fn repl() {
    let (env, runtime_predef) = init_env();

    let mut trans = Translate::new(env);

    let mut vm = VirtualMachine::new(vec![], runtime_predef);

    let mut rl = Editor::<()>::new();
    if rl.load_history("repl.hist.txt").is_err() {}

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let src = Source::from(line);

                let val = TrackedSexpr::from_source(&src)
                    .and_then(|sexpr| trans.objectify_toplevel(&sexpr).map_err(Into::into))
                    .and_then(|ast| {
                        let ast = ast.transform(&mut Boxify);
                        let ast = ast.transform(&mut Flatten::new());

                        let globals = trans.env.globals.clone();
                        let predef = trans.env.predef.clone();
                        println!("{:#?}", ast);

                        let code = BytecodeGenerator::compile_toplevel(&ast, globals, predef);
                        println!("{:?}", code);

                        //trans.global_env.update_runtime_globals(&mut sg);
                        //ast.eval(sr, sg);

                        let code = Box::leak(Box::new(code));
                        let closure = Box::leak(Box::new(Closure::simple(code)));

                        vm.resize_globals(trans.env.globals.len());

                        Ok(vm.eval(closure)?)
                    });

                match val {
                    Ok(x) => println!("{}", x),
                    Err(e) => report_error(e),
                }
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

pub fn report_error(e: Error) {
    match e.context {
        ErrorContext::None | ErrorContext::Source(SourceLocation::NoSource) => {
            eprintln!("Error: {:?}", e.kind)
        }
        ErrorContext::Source(SourceLocation::Span(span)) => {
            eprintln!("Error:");
            eprintln!("{}", span);
            eprintln!("{}", e.kind);
        }
    }
}
