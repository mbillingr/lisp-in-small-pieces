use crate::bytecode::Closure;
use crate::scm::Scm;

#[derive(Debug)]
pub struct Continuation {
    pub value_stack: Vec<Scm>,
    pub call_stack: Vec<(usize, isize, &'static Closure)>,
}

impl Continuation {
    pub fn new(value_stack: Vec<Scm>, call_stack: Vec<(usize, isize, &'static Closure)>) -> Self {
        Continuation {
            value_stack,
            call_stack,
        }
    }
}
