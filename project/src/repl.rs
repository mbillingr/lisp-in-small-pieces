use crate::error::{Error, ErrorContext};
use crate::language::scheme::{create_scheme_base_library, create_scheme_extra_library, Context};
use crate::source::SourceLocation;
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn repl() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("repl.hist.txt").is_err() {}

    let mut context = Context::new();

    context.add_library("sunny/base", create_scheme_base_library());
    context.add_library("sunny/extra", create_scheme_extra_library());

    context.import_library("sunny/base");

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                if line.starts_with('[') {
                    match line.as_str() {
                        "[globals]" => {
                            for (i, var) in context.trans().env.variables().enumerate() {
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
