use crate::ast::{
    Ast, AstNode, FixLet, Function, LocalAssignment, LocalReference, Ref, Sequence, Transformer,
    Variable, Visited,
};
use crate::env::{GlobalRuntimeEnv, LexicalRuntimeEnv};
use crate::source::SourceLocation;
use crate::value::Value;

#[derive(Debug, Clone)]
struct BoxRead {
    pub reference: AstNode,
    span: SourceLocation,
}

#[derive(Debug, Clone)]
struct BoxWrite {
    pub reference: AstNode,
    pub form: AstNode,
    span: SourceLocation,
}

#[derive(Debug, Clone)]
struct BoxCreate {
    pub variable: Variable,
    span: SourceLocation,
}

pub struct Boxify;

impl Transformer for Boxify {
    fn visit(&mut self, node: &AstNode) -> Visited {
        dispatch! { self on node:
            LocalReference => Boxify::transform_local_reference,
            LocalAssignment => Boxify::transform_local_assignment,
            Function => Boxify::transform_function,
            FixLet => Boxify::transform_fixlet,
        }
    }
}

impl Boxify {
    fn transform_local_reference(&self, node: &LocalReference) -> Visited {
        if node.variable().is_mutable() {
            let newnode: AstNode = BoxRead::new(Ref::new(node.clone()), node.source().clone());
            newnode.into()
        } else {
            Visited::Identity
        }
    }

    fn transform_local_assignment(&self, node: &LocalAssignment) -> AstNode {
        BoxWrite::new(
            Ref::new(node.reference.clone()),
            node.form.clone(),
            node.source().clone(),
        )
    }

    fn transform_function(&mut self, node: &Function) -> AstNode {
        Function::new(
            node.variables.clone(),
            boxify_mutable_variables(node.body.clone(), &node.variables).transform(self),
            node.source().clone(),
        )
    }

    fn transform_fixlet(&mut self, node: &FixLet) -> AstNode {
        FixLet::new(
            node.variables.clone(),
            node.arguments
                .iter()
                .map(|a| a.clone().transform(self))
                .collect(),
            boxify_mutable_variables(node.body.clone(), &node.variables).transform(self),
            node.source().clone(),
        )
    }
}

impl BoxRead {
    pub fn new(reference: AstNode, span: SourceLocation) -> Ref<Self> {
        Ref::new(BoxRead { reference, span })
    }
}

impl Ast for BoxRead {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.reference = self.reference.transform(visitor);
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, sr: &LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        self.reference.eval(sr, sg).get()
    }
}

impl BoxWrite {
    pub fn new(reference: AstNode, form: AstNode, span: SourceLocation) -> Ref<Self> {
        Ref::new(BoxWrite {
            reference,
            form,
            span,
        })
    }
}

impl Ast for BoxWrite {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.reference = self.reference.transform(visitor);
        self.form = self.form.transform(visitor);
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, sr: &LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        self.reference.eval(sr, sg).set(self.form.eval(sr, sg));
        Value::Undefined
    }
}

impl BoxCreate {
    pub fn new(variable: Variable, span: SourceLocation) -> Ref<Self> {
        Ref::new(BoxCreate { variable, span })
    }
}

impl Ast for BoxCreate {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(self: Ref<Self>, _visitor: &mut dyn Transformer) -> AstNode {
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, sr: &LexicalRuntimeEnv, _sg: &mut GlobalRuntimeEnv) -> Value {
        let x = sr.get_lexical(self.variable.name());
        unsafe {
            sr.set_lexical(self.variable.name(), Value::boxed(x));
        }
        Value::Undefined
    }
}

fn boxify_mutable_variables(mut body: AstNode, variables: &[Variable]) -> AstNode {
    for var in variables {
        if var.is_mutable() {
            let span = body.source().clone();
            body = Sequence::new(
                BoxCreate::new(var.clone(), SourceLocation::NoSource),
                body,
                span,
            );
        }
    }
    body
}
