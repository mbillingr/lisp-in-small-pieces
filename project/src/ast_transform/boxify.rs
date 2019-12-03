use crate::ast::{
    Ast, AstNode, FixLet, Function, LocalAssignment, LocalReference, Ref, Sequence, Transformer,
    Variable, Visited,
};
use crate::env::{GlobalRuntimeEnv, LexicalRuntimeEnv};
use crate::source::SourceLocation;
use crate::value::Value;

#[derive(Debug, Clone)]
struct BoxRead {
    pub reference: LocalReference,
    span: SourceLocation,
}

#[derive(Debug, Clone)]
struct BoxWrite {
    pub reference: LocalReference,
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
        if let Some(r) = node.downcast_ref::<LocalReference>() {
            Visited::Transformed(BoxRead::new(r.clone(), r.source().clone()))
        } else if let Some(a) = node.downcast_ref::<LocalAssignment>() {
            Visited::Transformed(BoxWrite::new(
                a.reference.clone(),
                a.form.clone(),
                a.source().clone(),
            ))
        } else if let Some(f) = node.downcast_ref::<Function>() {
            Visited::Transformed(Function::new(
                f.variables.clone(),
                boxify_mutable_variables(f.body.clone(), &f.variables).transform(self),
                f.source().clone(),
            ))
        } else if let Some(f) = node.downcast_ref::<FixLet>() {
            Visited::Transformed(FixLet::new(
                f.variables.clone(),
                f.arguments
                    .iter()
                    .map(|a| a.clone().transform(self))
                    .collect(),
                boxify_mutable_variables(f.body.clone(), &f.variables).transform(self),
                f.source().clone(),
            ))
        } else {
            Visited::Identity
        }
    }
}

impl BoxRead {
    pub fn new(reference: LocalReference, span: SourceLocation) -> Ref<Self> {
        Ref::new(BoxRead { reference, span })
    }
}

impl Ast for BoxRead {
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
        sr.get_lexical(self.reference.variable().name()).get()
    }
}

impl BoxWrite {
    pub fn new(reference: LocalReference, form: AstNode, span: SourceLocation) -> Ref<Self> {
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
        self.form = self.form.transform(visitor);
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, sr: &LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        sr.get_lexical(self.reference.variable().name())
            .set(self.form.eval(sr, sg));
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
