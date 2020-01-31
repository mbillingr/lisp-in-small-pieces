pub mod boxify;
pub mod flatten_closures;
pub mod generate_bytecode;

use super::syntax::Expression;

pub trait Transformer {
    fn visit(&mut self, expr: Expression) -> Visited;
}

pub enum Visited {
    Transformed(Expression),
    Recurse(Expression),
}

impl<T: Into<Expression>> From<T> for Visited {
    fn from(x: T) -> Self {
        Visited::Transformed(x.into())
    }
}
