#[derive(Debug, Copy, Clone)]
pub enum Arity {
    Exact(u16),
    AtLeast(u16),
}

impl Arity {
    pub fn check(&self, n_args: usize) -> bool {
        match *self {
            Arity::Exact(n) => n_args == n as usize,
            Arity::AtLeast(n) => n_args >= n as usize,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FunctionDescription {
    pub arity: Arity,
    pub text: &'static str,
}

impl FunctionDescription {
    pub fn new(arity: Arity, text: &'static str) -> Self {
        FunctionDescription { arity, text }
    }
}
