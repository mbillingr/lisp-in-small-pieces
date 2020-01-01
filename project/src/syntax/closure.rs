use super::expression::Expression;
use super::function::Function;
use super::reference::LocalReference;

#[derive(Debug, Clone)]
pub struct FlatClosure {
    pub func: Function,
    pub free_vars: Vec<LocalReference>,
}

impl FlatClosure {
    pub fn adjoin_free_variables(&mut self, node: &LocalReference) {
        if self.free_vars.iter().find(|&fv| fv == node).is_none() {
            self.free_vars.push(node.clone());
        }
    }
}
