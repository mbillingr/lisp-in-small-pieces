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

use crate::description::{Arity, FunctionDescription};
use crate::language::scheme::{expand_assign, expand_begin, expand_lambda};
use crate::objectify::Translate;
use crate::source::SourceLocation::NoSource;
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

    repl()
}
