#[macro_use]
mod utils;

#[macro_use]
mod library;

mod ast_transform;
pub mod interpreter;
mod continuation;
mod env;
mod error;
mod eval;
mod language;
mod macro_language;
mod objectify;
mod ports;
mod primitive;
mod repl;
mod scan_out_defines;
mod scm;
mod scm_write;
mod sexpr;
mod syntactic_closure;
mod syntax;

use bdwgc_alloc::Allocator;
use repl::repl;

#[cfg(not(test))]
#[global_allocator]
static GLOBAL_ALLOCATOR: Allocator = Allocator;

fn main() {
    unsafe { Allocator::initialize() }

    repl()
}
