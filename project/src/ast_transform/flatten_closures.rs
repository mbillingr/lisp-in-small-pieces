use super::{Transformer, Visited};
use crate::syntax::{
    Expression, FixLet, FlatClosure, FreeReference, FreeVariable, Function, LocalReference,
    LocalVariable,
};
use crate::utils::Named;
use std::convert::TryInto;

pub struct Flatten {
    current_function: Option<FlatClosure>,
    bound_vars: Vec<LocalVariable>,
}

impl Transformer for Flatten {
    fn visit(&mut self, expr: Expression) -> Visited {
        use crate::syntax::Reference::*;
        use Expression::*;
        match expr {
            Reference(LocalReference(x)) => self.local_reference(x),
            FixLet(x) => self.fixlet(x).into(),
            Function(x) => self.function(x).into(),
            x => Visited::Recurse(x),
        }
    }
}

impl Flatten {
    pub fn new() -> Self {
        Flatten {
            current_function: None,
            bound_vars: vec![],
        }
    }

    fn local_reference(&mut self, node: LocalReference) -> Visited {
        if self.bound_vars.contains(&node.var) {
            Visited::Recurse(node.into())
        } else {
            self.current_function
                .as_mut()
                .unwrap()
                .adjoin_free_variables(&node);
            Visited::Transformed(
                FreeReference::new(FreeVariable::new(node.var.name()), node.span).into(),
            )
        }
    }

    fn fixlet(&mut self, node: FixLet) -> FixLet {
        let arguments = node
            .arguments
            .iter()
            .map(|a| a.clone().transform(self))
            .collect();

        let n = self.bound_vars.len();
        self.bound_vars.extend_from_slice(&node.variables);

        let body = node.body.clone().transform(self);

        self.bound_vars.truncate(n);

        FixLet::new(node.variables.clone(), arguments, body, node.span)
    }

    fn function(&mut self, node: Function) -> FlatClosure {
        let newfun = FlatClosure {
            func: node.clone(),
            free_vars: vec![],
        };

        let mut trans = Flatten {
            current_function: Some(newfun),
            bound_vars: node.variables.clone(),
        };

        let newbody = node.body.transform(&mut trans);

        let mut newfun = trans.current_function.unwrap();

        let free_vars: Vec<_> = newfun
            .free_vars
            .iter()
            .cloned()
            .map(Expression::from)
            .map(|r| r.transform(self))
            .map(Expression::try_into)
            .map(|r| r.expect("expected local reference"))
            .collect();

        newfun.func.body = Box::new(newbody);
        newfun.free_vars = free_vars;

        newfun
    }
}
