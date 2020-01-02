use super::expression::Expression;
use super::function::Function;
use super::reference::LocalReference;
use crate::ast_transform::Transformer;
use std::convert::TryInto;

#[derive(Debug, Clone)]
pub struct FlatClosure {
    pub func: Function,
    pub free_vars: Vec<LocalReference>,
}

impl_sourced!(FlatClosure: self.func.span);

impl FlatClosure {
    pub fn adjoin_free_variables(&mut self, node: &LocalReference) {
        if self.free_vars.iter().find(|&fv| fv == node).is_none() {
            self.free_vars.push(node.clone());
        }
    }

    pub fn default_transform(mut self, visitor: &mut impl Transformer) -> Self {
        *self.func.body = self.func.body.transform(visitor);
        self.free_vars = self
            .free_vars
            .into_iter()
            .map(Expression::from)
            .map(|fv| {
                fv.transform(visitor)
                    .try_into()
                    .expect("Expected Local Reference")
            })
            .collect();
        self
    }
}
