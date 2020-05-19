use crate::error::{Error, ErrorContext};
use crate::language::scheme::{
    create_scheme_base_library, create_scheme_extra_library, create_scheme_ports_library, Context,
};
use crate::scm::Scm;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::path::Path;
use sunny_parser::{Source, SourceLocation};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

pub fn repl() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("repl.hist.txt").is_err() {}

    let mut context = Context::new();

    context.add_library("sunny/core", create_scheme_base_library());
    context.add_library("sunny/extra", create_scheme_extra_library());
    context.add_library("sunny/ports-core", create_scheme_ports_library());

    context.import_library("scheme/base");

    println!("Sunny Scheme REPL version {}.", VERSION);

    for arg in std::env::args().skip(1) {
        match Source::from_file(Path::new(&arg))
            .map_err(Error::from)
            .and_then(|src| context.eval_source(&src))
        {
            Ok(_) => {}
            Err(e) => {
                report_error(e);
                return;
            }
        }
    }

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
                        Ok(Scm::Undefined) => {}
                        Ok(x) => println!("{}", x.write()),
                        Err(e) => report_error(e),
                    }

                    if !context.vm.value_stack.is_empty() {
                        eprintln!(
                            "WARNING: value stack not empty\n    {:?}",
                            context.vm.value_stack
                        );
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
            eprintln!("{}", e.kind)
        }
        ErrorContext::Source(SourceLocation::Span(span)) => {
            eprintln!("Error:");
            eprintln!("{}", span);
            eprintln!("{}", e.kind);
        }
    }
}
