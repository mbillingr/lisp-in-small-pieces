use crate::ast_transform::boxify::Boxify;
use crate::ast_transform::flatten_closures::Flatten;
use crate::ast_transform::generate_bytecode::BytecodeGenerator;
use crate::bytecode::{Closure, VirtualMachine};
use crate::{
    ast::{Arity, FunctionDescription, MagicKeyword, RuntimePrimitive, Variable},
    env::{Env, EnvAccess, EnvChain, GlobalRuntimeEnv},
    error::{Error, ErrorContext},
    language::scheme::Context,
    objectify::Translate,
    scm::Scm,
    sexpr::TrackedSexpr,
    source::{Source, SourceLocation},
};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::collections::HashMap;

pub fn repl() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("repl.hist.txt").is_err() {}

    let mut context = Context::new();

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                match context.eval_str(&line) {
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
