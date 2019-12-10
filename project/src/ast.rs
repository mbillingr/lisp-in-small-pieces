use crate::env::{Env, GlobalRuntimeEnv, LexicalRuntimeEnv};
use crate::objectify::{Result, Translate};
use crate::sexpr::TrackedSexpr as Sexpr;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::value::Value;
use downcast_rs::{impl_downcast, Downcast};
use std::cell::Cell;
use std::rc::Rc;

pub type Ref<T> = Box<T>;

pub type AstNode = Ref<dyn Ast>;

pub trait Ast: std::fmt::Debug + Downcast {
    fn source(&self) -> &SourceLocation;

    fn default_transform(self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode;

    fn deep_clone(&self) -> AstNode {
        unimplemented!("deep clone {:?}", self)
    }

    fn eval(&self, _sr: &LexicalRuntimeEnv, _sg: &mut GlobalRuntimeEnv) -> Value {
        unimplemented!("eval {:?}", self)
    }
}

impl_downcast!(Ast);

impl dyn Ast {
    pub fn transform(self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        match visitor.visit(&self) {
            Visited::Transformed(node) => node,
            Visited::Identity => self.default_transform(visitor),
        }
    }
}

impl Clone for Ref<dyn Ast> {
    fn clone(&self) -> Self {
        self.deep_clone()
    }
}

pub trait Transformer {
    fn visit(&mut self, node: &AstNode) -> Visited;
}

pub enum Visited {
    Identity,
    Transformed(AstNode),
}

impl From<AstNode> for Visited {
    fn from(node: AstNode) -> Self {
        Visited::Transformed(node)
    }
}

pub type MagicKeywordHandler = fn(&mut Translate, &Sexpr, &Env) -> Result<AstNode>;

#[derive(Clone)]
pub struct MagicKeyword {
    pub name: Symbol,
    pub handler: MagicKeywordHandler,
}

impl std::fmt::Debug for MagicKeyword {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl MagicKeyword {
    pub fn new(name: impl Into<Symbol>, handler: MagicKeywordHandler) -> Rc<Self> {
        Rc::new(MagicKeyword {
            name: name.into(),
            handler,
        })
    }

    pub fn name(&self) -> &Symbol {
        &self.name
    }
}

impl Ast for MagicKeyword {
    fn source(&self) -> &SourceLocation {
        &SourceLocation::NoSource
    }

    fn default_transform(self: Ref<Self>, _visitor: &mut dyn Transformer) -> AstNode {
        unreachable!()
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }
}

#[derive(Clone)]
pub enum Variable {
    Local(Rc<(Symbol, Cell<bool>, Cell<bool>)>),
    Global(Symbol),
    Predefined(Rc<(Symbol, FunctionDescription)>),
    Macro(Rc<MagicKeyword>),
    Free(Symbol),
}

impl Variable {
    pub fn local(name: impl Into<Symbol>, mutable: bool, dotted: bool) -> Self {
        Variable::Local(Rc::new((
            name.into(),
            Cell::new(mutable),
            Cell::new(dotted),
        )))
    }

    pub fn global(name: impl Into<Symbol>) -> Self {
        Variable::Global(name.into())
    }

    pub fn predefined(name: impl Into<Symbol>, func: FunctionDescription) -> Self {
        Variable::Predefined(Rc::new((name.into(), func)))
    }

    pub fn is_same(&self, other: &Self) -> bool {
        use Variable::*;
        match (self, other) {
            (Local(a), Local(b)) => Rc::ptr_eq(a, b),
            (Global(a), Global(b)) => a == b,
            (Predefined(a), Predefined(b)) => Rc::ptr_eq(a, b),
            (Macro(a), Macro(b)) => Rc::ptr_eq(a, b),
            (Free(a), Free(b)) => a == b,
            (_, _) => false,
        }
    }

    pub fn name(&self) -> &Symbol {
        match self {
            Variable::Local(v) => &v.0,
            Variable::Global(v) => &*v,
            Variable::Predefined(v) => &v.0,
            Variable::Macro(mkw) => mkw.name(),
            Variable::Free(v) => &*v,
        }
    }

    pub fn is_dotted(&self) -> bool {
        match self {
            Variable::Local(v) => v.2.get(),
            _ => panic!("invalid check for dotted variable"),
        }
    }

    pub fn set_dotted(&self, d: bool) {
        match self {
            Variable::Local(v) => v.2.set(d),
            _ => panic!("attempt to set dotted on non-local variable"),
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            Variable::Local(v) => v.1.get(),
            _ => panic!("invalid check for mutable variable"),
        }
    }

    pub fn set_mutable(&self, d: bool) {
        match self {
            Variable::Local(v) => v.1.set(d),
            _ => panic!("attempt to set mutable on non-local variable"),
        }
    }

    pub fn description(&self) -> &FunctionDescription {
        match self {
            Variable::Predefined(v) => &v.1,
            _ => panic!("attempt to get description of non-predefined variable"),
        }
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.is_same(other)
    }
}

impl std::fmt::Debug for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Variable::Local(var) => write!(
                f,
                "local{}{} {}",
                if var.1.get() { " mut" } else { "" },
                if var.2.get() { " dotted" } else { "" },
                var.0
            ),
            Variable::Global(name) => write!(f, "global {}", name),
            Variable::Predefined(var) => write!(f, "predefined {}", var.0),
            Variable::Macro(var) => write!(f, "macro {}", var.name),
            Variable::Free(name) => write!(f, "free {}", name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalReference {
    var: Variable,
    span: SourceLocation,
}

impl PartialEq for LocalReference {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
    }
}

impl LocalReference {
    pub fn new(var: Variable, span: SourceLocation) -> Ref<Self> {
        Ref::new(LocalReference { var, span })
    }

    pub fn variable(&self) -> &Variable {
        &self.var
    }
}

impl Ast for LocalReference {
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
        sr.get_lexical(self.var.name())
    }
}

#[derive(Debug, Clone)]
pub struct GlobalReference {
    var: Variable,
    span: SourceLocation,
}

impl GlobalReference {
    pub fn new(var: Variable, span: SourceLocation) -> Ref<Self> {
        Ref::new(GlobalReference { var, span })
    }

    pub fn variable(&self) -> &Variable {
        &self.var
    }
}

impl Ast for GlobalReference {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(self: Ref<Self>, _visitor: &mut dyn Transformer) -> AstNode {
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, _sr: &LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        sg.get_global(self.var.name())
    }
}

#[derive(Debug, Clone)]
pub struct PredefinedReference {
    var: Variable,
    span: SourceLocation,
}

impl PredefinedReference {
    pub fn new(var: Variable, span: SourceLocation) -> Ref<Self> {
        Ref::new(PredefinedReference { var, span })
    }

    pub fn variable(&self) -> &Variable {
        &self.var
    }
}

impl Ast for PredefinedReference {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(self: Ref<Self>, _visitor: &mut dyn Transformer) -> AstNode {
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, _sr: &LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        (*sg.get_predefined(self.var.name())).into()
    }
}

#[derive(Debug, Clone)]
pub struct LocalAssignment {
    pub reference: LocalReference,
    pub form: AstNode,
    span: SourceLocation,
}

impl LocalAssignment {
    pub fn new(reference: LocalReference, form: AstNode, span: SourceLocation) -> Ref<Self> {
        Ref::new(LocalAssignment {
            reference,
            form,
            span,
        })
    }
}

impl Ast for LocalAssignment {
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

    fn eval(&self, _sr: &LexicalRuntimeEnv, _sg: &mut GlobalRuntimeEnv) -> Value {
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
pub struct GlobalAssignment {
    variable: Variable,
    form: AstNode,
    span: SourceLocation,
}

impl GlobalAssignment {
    pub fn new(variable: Variable, form: AstNode, span: SourceLocation) -> Ref<Self> {
        Ref::new(GlobalAssignment {
            variable,
            form,
            span,
        })
    }
}

impl Ast for GlobalAssignment {
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
        let val = self.form.eval(sr, sg);
        sg.set_global(self.variable.name(), val);
        Value::Undefined
    }
}

#[derive(Debug, Clone)]
pub struct Constant {
    value: Value,
    span: SourceLocation,
}

impl Constant {
    pub fn new(value: impl Into<Value>, span: SourceLocation) -> Ref<Self> {
        Ref::new(Constant {
            value: value.into(),
            span,
        })
    }

    /*pub fn value(&self) -> &Value {
        &self.value
    }*/
}

impl Ast for Constant {
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
        self.value.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Sequence {
    first: AstNode,
    next: AstNode,
    span: SourceLocation,
}

impl Sequence {
    pub fn new(first: AstNode, next: AstNode, span: SourceLocation) -> Ref<Self> {
        Ref::new(Sequence { first, next, span })
    }
}

impl Ast for Sequence {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.first = self.first.transform(visitor);
        self.next = self.next.transform(visitor);
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, sr: &LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        self.first.eval(sr, sg);
        self.next.eval(sr, sg)
    }
}

#[derive(Debug, Clone)]
pub struct Alternative {
    condition: AstNode,
    consequence: AstNode,
    alternative: AstNode,
    span: SourceLocation,
}

impl Alternative {
    pub fn new(
        condition: AstNode,
        consequence: AstNode,
        alternative: AstNode,
        span: SourceLocation,
    ) -> Ref<Self> {
        Ref::new(Alternative {
            condition,
            consequence,
            alternative,
            span,
        })
    }
}

impl Ast for Alternative {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.condition = self.condition.transform(visitor);
        self.consequence = self.consequence.transform(visitor);
        self.alternative = self.alternative.transform(visitor);
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, sr: &LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        if self.condition.eval(sr, sg).is_true() {
            self.consequence.eval(sr, sg)
        } else {
            self.alternative.eval(sr, sg)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub variables: Vec<Variable>,
    pub body: AstNode,
    span: SourceLocation,
}

impl Function {
    pub fn new(variables: Vec<Variable>, body: AstNode, span: SourceLocation) -> Ref<Self> {
        Ref::new(Function {
            variables,
            body,
            span,
        })
    }
}

impl Ast for Function {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.body = self.body.transform(visitor);
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, sr: &LexicalRuntimeEnv, _sg: &mut GlobalRuntimeEnv) -> Value {
        Value::Procedure(RuntimeProcedure::new(
            self.body.clone(),
            self.variables.clone(),
            sr.clone(),
        ))
    }
}

#[derive(Debug)]
pub struct RegularApplication {
    function: AstNode,
    arguments: Vec<AstNode>,
    span: SourceLocation,
}

impl RegularApplication {
    pub fn new(function: AstNode, arguments: Vec<AstNode>, span: SourceLocation) -> Ref<Self> {
        Ref::new(RegularApplication {
            function,
            arguments,
            span,
        })
    }
}

impl Ast for RegularApplication {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.function = self.function.transform(visitor);
        self.arguments = self
            .arguments
            .into_iter()
            .map(|a| a.transform(visitor))
            .collect();
        self
    }

    fn eval(&self, sr: &LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        let func = self.function.eval(sr, sg);
        let args: Vec<_> = self.arguments.iter().map(|a| a.eval(sr, sg)).collect();
        func.invoke(args, sg)
    }
}

#[derive(Debug, Clone)]
pub struct PredefinedApplication {
    variable: Variable,
    arguments: Vec<AstNode>,
    span: SourceLocation,
}

impl PredefinedApplication {
    pub fn new(variable: Variable, arguments: Vec<AstNode>, span: SourceLocation) -> Ref<Self> {
        Ref::new(PredefinedApplication {
            variable,
            arguments,
            span,
        })
    }
}

impl Ast for PredefinedApplication {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.arguments = self
            .arguments
            .into_iter()
            .map(|a| a.transform(visitor))
            .collect();
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, sr: &LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        let name = self.variable.name();
        let args: Vec<_> = self.arguments.iter().map(|a| a.eval(sr, sg)).collect();
        let func = sg.get_predefined(name);
        func.invoke(args)
    }
}

#[derive(Debug, Clone)]
pub struct FixLet {
    pub variables: Vec<Variable>,
    pub arguments: Vec<AstNode>,
    pub body: AstNode,
    span: SourceLocation,
}

impl FixLet {
    pub fn new(
        variables: Vec<Variable>,
        arguments: Vec<AstNode>,
        body: AstNode,
        span: SourceLocation,
    ) -> Ref<Self> {
        Ref::new(FixLet {
            variables,
            arguments,
            body,
            span,
        })
    }
}

impl Ast for FixLet {
    fn source(&self) -> &SourceLocation {
        &self.span
    }

    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.arguments = self
            .arguments
            .into_iter()
            .map(|a| a.transform(visitor))
            .collect();
        self.body = self.body.transform(visitor);
        self
    }

    fn deep_clone(&self) -> AstNode {
        Ref::new(self.clone())
    }

    fn eval(&self, sr: &LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        let args: Vec<_> = self.arguments.iter().map(|x| x.eval(sr, sg)).collect();

        let mut sr = sr.clone();

        for (var, val) in self.variables.iter().zip(args) {
            sr = sr.extend(var.name().clone(), val);
        }

        let result = self.body.eval(&sr, sg);

        result
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Arity {
    Exact(u16),
    AtLeast(u16),
}

impl Arity {
    pub fn check(&self, n_args: usize) -> bool {
        match *self {
            Arity::Exact(n) => n_args == n as usize,
            Arity::AtLeast(n) => n_args >= n as usize,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FunctionDescription {
    pub arity: Arity,
    pub text: &'static str,
}

impl FunctionDescription {
    pub fn new(arity: Arity, text: &'static str) -> Self {
        FunctionDescription { arity, text }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct RuntimePrimitive {
    pub func: fn(args: Vec<Value>) -> Value,
    pub arity: Arity,
}

impl RuntimePrimitive {
    pub fn new(arity: Arity, func: fn(args: Vec<Value>) -> Value) -> Self {
        RuntimePrimitive { arity, func }
    }

    pub fn invoke(&self, args: Vec<Value>) -> Value {
        if self.arity.check(args.len()) {
            (self.func)(args)
        } else {
            panic!("Incorrect arity")
        }
    }
}

impl std::fmt::Display for RuntimePrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "*primitive*")
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeProcedure {
    pub body: AstNode,
    pub variables: Vec<Variable>,
    pub env: LexicalRuntimeEnv,
}

impl std::fmt::Display for RuntimeProcedure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let vars: Vec<_> = self
            .variables
            .iter()
            .map(|var| {
                if var.is_dotted() {
                    format!(" . {}", var.name())
                } else {
                    var.name().to_string()
                }
            })
            .collect();
        let vars = vars.join(" ");
        write!(f, "(lambda ({}) ...)", vars)
    }
}

impl RuntimeProcedure {
    pub fn new(body: AstNode, variables: Vec<Variable>, env: LexicalRuntimeEnv) -> Self {
        RuntimeProcedure {
            body,
            variables,
            env,
        }
    }

    pub fn invoke(&self, args: Vec<Value>, sg: &mut GlobalRuntimeEnv) -> Value {
        if self.variables.len() != args.len() {
            panic!("Incorrect arity")
        }

        let mut env = self.env.clone();
        for (p, a) in self.variables.iter().zip(args) {
            env = env.extend(p.name().clone(), a);
        }

        self.body.eval(&env, sg)
    }
}
