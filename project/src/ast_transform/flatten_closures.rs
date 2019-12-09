use crate::ast::{
    Ast, AstNode, FixLet, Function, LocalReference, Ref, Transformer, Variable, Visited,
};
use crate::env::{GlobalRuntimeEnv, LexicalRuntimeEnv};
use crate::source::SourceLocation;
use crate::value::Value;

#[derive(Debug, Clone)]
struct FlatClosure {
    func: Function,
    free_vars: Vec<Variable>,
}

#[derive(Debug, Clone)]
struct FreeReference {
    var: Variable,
    span: SourceLocation,
}

pub struct Flatten {
    current_function: Option<FlatClosure>,
    vars: Vec<Variable>,
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
    fn local_reference(&mut self, node: &LocalReference) -> Visited {
        if let Some(v) = self.vars.iter().rev().find(|&v| node.variable().is_same(v)) {
            self.current_function
                .as_mut()
                .unwrap()
                .adjoin_free_variables(node);
            Visited::Transformed(FreeReference::new(v.clone(), node.source().clone()))
        } else {
            Visited::Identity
        }
    }

    fn fixlet(&self, node: &FixLet) -> AstNode {
        unimplemented!()
    }

    fn function(&self, node: &Function) -> AstNode {
        unimplemented!()
    }
}

impl FlatClosure {
    fn adjoin_free_variables(&mut self, node: &LocalReference) {
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
