#[macro_use]
mod utils;

mod ast_transform;
mod bytecode;
mod description;
mod env;
mod error;
mod eval;
mod language;
mod objectify;
mod parsing;
mod primitive;
mod repl;
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
