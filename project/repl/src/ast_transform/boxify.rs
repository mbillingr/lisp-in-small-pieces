use super::{Transformer, Visited};
use crate::syntax::{
    BoxCreate, BoxRead, BoxWrite, Expression, FixLet, Function, LocalAssignment, LocalReference,
    LocalVariable, Sequence,
};
use sunny_parser::{SourceLocation, Sourced};

pub struct Boxify;

impl Transformer for Boxify {
    fn visit(&mut self, expr: Expression) -> Visited {
        use crate::syntax::Assignment::*;
        use crate::syntax::Reference::*;
        use Expression::*;
        match expr {
            Reference(LocalReference(x)) => self.transform_local_reference(x),
            Assignment(LocalAssignment(x)) => self.transform_local_assignment(x).into(),
            Function(x) => self.transform_function(x).into(),
            FixLet(x) => self.transform_fixlet(x).into(),
            x => Visited::Recurse(x),
        }
    }
}

impl Boxify {
    fn transform_local_reference(&self, node: LocalReference) -> Visited {
        if node.var.is_mutable() {
            let span = node.span.clone();
            let newnode = BoxRead::new(node.into(), span);
            newnode.into()
        } else {
            Visited::Recurse(node.into())
        }
    }

    fn transform_local_assignment(&mut self, node: LocalAssignment) -> BoxWrite {
        BoxWrite::new(node.reference.into(), node.form.transform(self), node.span)
    }

    fn transform_function(&mut self, node: Function) -> Function {
        let body = boxify_mutable_variables(*node.body, &node.variables).transform(self);
        Function::new(node.variables, body, node.span)
    }

    fn transform_fixlet(&mut self, node: FixLet) -> FixLet {
        FixLet::new(
            node.variables.clone(),
            node.arguments
                .iter()
                .map(|a| a.clone().transform(self))
                .collect(),
            boxify_mutable_variables(*node.body, &node.variables).transform(self),
            node.span,
        )
    }
}

fn boxify_mutable_variables(mut body: Expression, variables: &[LocalVariable]) -> Expression {
    for var in variables {
        if var.is_mutable() {
            let span = body.source().clone();
            body = Sequence::new(
                Expression::from(BoxCreate::new(var.clone(), SourceLocation::NoSource)),
                body,
                span,
            )
            .into();
        }
    }
    body
}
