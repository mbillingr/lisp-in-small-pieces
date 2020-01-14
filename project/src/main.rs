#[macro_use]
mod utils;

mod ast_transform;
pub mod bytecode;
mod description;
mod env;
mod error;
mod eval;
mod language;
mod library;
mod macro_language;
mod objectify;
mod parsing;
mod primitive;
mod repl;
mod scan_out_defines;
mod scm;
mod sexpr;
mod source;
mod symbol;
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
