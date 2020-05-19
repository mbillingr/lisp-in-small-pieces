use super::{Transformer, Visited};
use crate::syntax::{
    Expression, FixLet, FlatClosure, FreeReference, FreeVariable, Function, LetContinuation,
    Reference, Variable,
};
use std::convert::TryInto;
use sunny_common::Named;
use sunny_parser::Sourced;

pub struct Flatten {
    current_function: Option<FlatClosure>,
    bound_vars: Vec<Variable>,
}

impl Transformer for Flatten {
    fn visit(&mut self, expr: Expression) -> Visited {
        use crate::syntax::Reference::*;
        use Expression::*;
        match expr {
            Reference(r @ LocalReference(_)) => self.local_reference(r),
            FixLet(x) => self.fixlet(x).into(),
            Function(x) => self.function(x).into(),
            LetContinuation(l) => self.letcont(l).into(),
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

    fn local_reference(&mut self, node: Reference) -> Visited {
        let var = node.var();
        if self.bound_vars.contains(&var) {
            Visited::Recurse(node.into())
        } else {
            self.current_function
                .as_mut()
                .unwrap()
                .adjoin_free_variables(&node);
            Visited::Transformed(
                FreeReference::new(FreeVariable::new(var.name()), node.source().clone()).into(),
            )
        }
    }

    fn letcont(&mut self, node: LetContinuation) -> LetContinuation {
        self.bound_vars.push(node.variable.clone().into());
        let body = node.body.transform(self);
        self.bound_vars.pop();
        LetContinuation::new(node.kind, node.variable, body, node.span)
    }

    fn fixlet(&mut self, node: FixLet) -> FixLet {
        let arguments = node
            .arguments
            .iter()
            .map(|a| a.clone().transform(self))
            .collect();

        let n = self.bound_vars.len();
        self.bound_vars
            .extend(node.variables.iter().cloned().map(Variable::from));

        let body = node.body.transform(self);

        self.bound_vars.truncate(n);

        FixLet::new(node.variables, arguments, body, node.span)
    }

    fn function(&mut self, node: Function) -> FlatClosure {
        let newfun = FlatClosure {
            func: node.clone(),
            free_vars: vec![],
        };

        let mut trans = Flatten {
            current_function: Some(newfun),
            bound_vars: node.variables.iter().cloned().map(Variable::from).collect(),
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
            .map(|r| r.expect("expected reference"))
            .collect();

        newfun.func.body = Box::new(newbody);
        newfun.free_vars = free_vars;

        newfun
    }
}
