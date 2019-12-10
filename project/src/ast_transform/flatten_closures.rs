use crate::ast::{
    Ast, AstNode, FixLet, Function, LocalReference, Ref, Transformer, Variable, Visited,
};
use crate::env::{GlobalRuntimeEnv, LexicalRuntimeEnv};
use crate::source::SourceLocation;
use crate::value::Value;

#[derive(Debug, Clone)]
struct FlatClosure {
    pub func: Function,
    pub free_vars: Vec<AstNode>,
}

#[derive(Debug, Clone)]
struct FreeReference {
    var: Variable,
    span: SourceLocation,
}

pub struct Flatten {
    current_function: Option<FlatClosure>,
    bound_vars: Vec<Variable>,
}

impl Transformer for Flatten {
    fn visit(&mut self, node: &AstNode) -> Visited {
        dispatch! { self on node:
            LocalReference => Flatten::local_reference,
            FixLet => Flatten::fixlet,
            Function => Flatten::function,
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

    fn local_reference(&mut self, node: &LocalReference) -> Visited {
        if self.bound_vars.contains(node.variable()) {
            Visited::Identity
        } else {
            self.current_function
                .as_mut()
                .unwrap()
                .adjoin_free_variables(node);
            Visited::Transformed(FreeReference::new(
                Variable::Free(*node.variable().name()),
                node.source().clone(),
            ))
        }
    }

    fn fixlet(&mut self, node: &FixLet) -> AstNode {
        let arguments = node
            .arguments
            .iter()
            .map(|a| a.clone().transform(self))
            .collect();

        let n = self.bound_vars.len();
        self.bound_vars.extend_from_slice(&node.variables);

        let body = node.body.clone().transform(self);

        self.bound_vars.truncate(n);

        FixLet::new(
            node.variables.clone(),
            arguments,
            body,
            node.source().clone(),
        )
    }

    fn function(&mut self, node: &Function) -> AstNode {
        let newfun = FlatClosure {
            func: node.clone(),
            free_vars: vec![],
        };

        let mut trans = Flatten {
            current_function: Some(newfun),
            bound_vars: node.variables.clone(),
        };

        let newbody = node.body.clone().transform(&mut trans);

        let mut newfun = trans.current_function.unwrap();

        let free_vars: Vec<_> = newfun
            .free_vars
            .iter()
            .cloned()
            .map(|r| r.transform(self))
            .collect();

        newfun.func.body = newbody;
        newfun.free_vars = free_vars;

        Ref::new(newfun)
    }
}

impl FlatClosure {
    fn adjoin_free_variables(&mut self, node: &LocalReference) {
        if self
            .free_vars
            .iter()
            .find(|fv| fv.downcast_ref::<LocalReference>() == Some(node))
            .is_none()
        {
            self.free_vars.push(Ref::new(node.clone()));
        }
    }
}

impl Ast for FlatClosure {
    fn source(&self) -> &SourceLocation {
        self.func.source()
    }

    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.func.body = self.func.body.transform(visitor);
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, _sr: &LexicalRuntimeEnv, _sg: &mut GlobalRuntimeEnv) -> Value {
        unimplemented!()
    }
}

impl FreeReference {
    fn new(var: Variable, span: SourceLocation) -> AstNode {
        Ref::new(FreeReference { var, span })
    }
}

impl Ast for FreeReference {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(self: Ref<Self>, _visitor: &mut dyn Transformer) -> AstNode {
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, _sr: &LexicalRuntimeEnv, _sg: &mut GlobalRuntimeEnv) -> Value {
        unimplemented!()
    }
}
