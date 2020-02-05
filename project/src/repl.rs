use crate::error::{Error, ErrorContext};
use crate::language::scheme::{create_scheme_base_library, Context};
use crate::source::SourceLocation;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::path::Path;

pub fn repl() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("repl.hist.txt").is_err() {}

    let mut context = Context::new();

    context.add_library("scheme/base", create_scheme_base_library());
    context.import_library("scheme/base");

    context.build_library(Path::new("testing/lib")).unwrap();

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                if line.starts_with('[') {
                    match line.as_str() {
                        "[globals]" => {
                            for (i, var) in context.trans.env.variables().enumerate() {
                                println!("{:3} {:?}", i, var)
                            }
                        }
                        _ => {}
                    }
                } else {
                    match context.eval_str(&line) {
                        Ok(x) => println!("{}", x),
                        Err(e) => report_error(e),
                    }
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
