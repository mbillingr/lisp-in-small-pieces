use crate::env::{Env, LexicalRuntimeEnv, GlobalRuntimeEnv};
use crate::objectify::{Result, Translate};
use crate::value::{Symbol, Value};
use downcast_rs::{impl_downcast, Downcast};
use std::cell::Cell;
use crate::sexpr::Sexpr;

type Ref<T> = Box<T>;

pub type AstNode = Ref<dyn Ast>;

pub trait Ast: std::fmt::Debug + Downcast {
    fn default_transform(self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode;

    fn eval(&self, sr: &mut LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
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

pub trait Transformer {
    fn visit(&mut self, node: &AstNode) -> Visited;
}

pub enum Visited {
    Identity,
    Transformed(AstNode),
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
    pub fn new(name: Symbol, handler: MagicKeywordHandler) -> Ref<Self> {
        Ref::new(MagicKeyword { name, handler })
    }

    pub fn name(&self) -> Symbol {
        self.name
    }
}

impl Ast for MagicKeyword {
    fn default_transform(self: Ref<Self>, _visitor: &mut dyn Transformer) -> AstNode {
        unreachable!()
    }
}

#[derive(Debug, Clone)]
pub enum Variable {
    Local(Symbol, Cell<bool>, Cell<bool>),
    Global(Symbol),
    Predefined(Symbol, FunctionDescription),
    Macro(Ref<MagicKeyword>),
}

impl Variable {
    pub fn local(name: Symbol, mutable: bool, dotted: bool) -> Self {
        Variable::Local(name, Cell::new(mutable), Cell::new(dotted))
    }

    pub fn name(&self) -> Symbol {
        match self {
            Variable::Local(n, _, _) | Variable::Global(n) | Variable::Predefined(n, _) => n,
            Variable::Macro(mkw) => mkw.name(),
        }
    }

    pub fn is_dotted(&self) -> bool {
        match self {
            Variable::Local(_, _, dotted) => dotted.get(),
            _ => panic!("invalid check for dotted variable"),
        }
    }

    pub fn set_dotted(&self, d: bool) {
        match self {
            Variable::Local(_, _, dotted) => dotted.set(d),
            _ => panic!("attempt to set dotted on non-local variable"),
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            Variable::Local(_, mutable, _) => mutable.get(),
            _ => panic!("invalid check for mutable variable"),
        }
    }

    pub fn set_mutable(&self, d: bool) {
        match self {
            Variable::Local(_, mutable, _) => mutable.set(d),
            _ => panic!("attempt to set mutable on non-local variable"),
        }
    }

    pub fn description(&self) -> &FunctionDescription {
        match self {
            Variable::Predefined(_, descr) => descr,
            _ => panic!("attempt to get description of non-predefined variable"),
        }
    }
}

#[derive(Debug)]
pub struct LocalReference(Variable);

impl LocalReference {
    pub fn new(var: Variable) -> Ref<Self> {
        Ref::new(LocalReference(var))
    }

    pub fn variable(&self) -> &Variable {
        &self.0
    }
}

impl Ast for LocalReference {
    fn default_transform(self: Ref<Self>, _visitor: &mut dyn Transformer) -> AstNode {
        self
    }

    fn eval(&self, sr: &mut LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        sr.get_lexical(self.0.name())
    }
}

#[derive(Debug)]
pub struct GlobalReference(Variable);

impl GlobalReference {
    pub fn new(var: Variable) -> Ref<Self> {
        Ref::new(GlobalReference(var))
    }

    pub fn variable(&self) -> &Variable {
        &self.0
    }
}

impl Ast for GlobalReference {
    fn default_transform(self: Ref<Self>, _visitor: &mut dyn Transformer) -> AstNode {
        self
    }

    fn eval(&self, sr: &mut LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        sg.get_global(self.0.name())
    }
}

#[derive(Debug)]
pub struct PredefinedReference(Variable);

impl PredefinedReference {
    pub fn new(var: Variable) -> Ref<Self> {
        Ref::new(PredefinedReference(var))
    }

    pub fn variable(&self) -> &Variable {
        &self.0
    }
}

impl Ast for PredefinedReference {
    fn default_transform(self: Ref<Self>, _visitor: &mut dyn Transformer) -> AstNode {
        self
    }
}

#[derive(Debug)]
pub struct LocalAssignment {
    reference: LocalReference,
    form: AstNode,
}

impl LocalAssignment {
    pub fn new(reference: LocalReference, form: AstNode) -> Ref<Self> {
        Ref::new(LocalAssignment{
            reference, form
        })
    }
}

impl Ast for LocalAssignment {
    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.form = self.form.transform(visitor);
        self
    }

    fn eval(&self, sr: &mut LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct GlobalAssignment {
    variable: Variable,
    form: AstNode,
}

impl GlobalAssignment {
    pub fn new(variable: Variable, form: AstNode) -> Ref<Self> {
        Ref::new(GlobalAssignment{
            variable, form
        })
    }
}

impl Ast for GlobalAssignment {
    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.form = self.form.transform(visitor);
        self
    }

    fn eval(&self, sr: &mut LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        let val = self.form.eval(sr, sg);
        sg.set_global(self.variable.name(), val);
        Value::Undefined
    }
}

#[derive(Debug)]
pub struct Constant(Value);

impl Constant {
    pub fn new(value: impl Into<Value>) -> Ref<Self> {
        Ref::new(Constant(value.into()))
    }

    pub fn value(&self) -> &Value {
        &self.0
    }
}

impl Ast for Constant {
    fn default_transform(self: Ref<Self>, _visitor: &mut dyn Transformer) -> AstNode {
        self
    }

    fn eval(&self, sr: &mut LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        self.0.clone()
    }
}

#[derive(Debug)]
pub struct Sequence {
    first: AstNode,
    next: AstNode,
}

impl Sequence {
    pub fn new(first: AstNode, next: AstNode) -> Ref<Self> {
        Box::new(Sequence { first, next })
    }
}

impl Ast for Sequence {
    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.first = self.first.transform(visitor);
        self.next = self.next.transform(visitor);
        self
    }

    fn eval(&self, sr: &mut LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        self.first.eval(sr, sg);
        self.next.eval(sr, sg)
    }
}

#[derive(Debug)]
pub struct Alternative {
    condition: AstNode,
    consequence: AstNode,
    alternative: AstNode,
}

impl Alternative {
    pub fn new(condition: AstNode, consequence: AstNode, alternative: AstNode) -> Ref<Self> {
        Box::new(Alternative {
            condition,
            consequence,
            alternative,
        })
    }
}

impl Ast for Alternative {
    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.condition = self.condition.transform(visitor);
        self.consequence = self.consequence.transform(visitor);
        self.alternative = self.alternative.transform(visitor);
        self
    }
}

#[derive(Debug)]
pub struct Function {
    pub variables: Vec<Variable>,
    pub body: AstNode,
}

impl Function {
    pub fn new(variables: Vec<Variable>, body: AstNode) -> Ref<Self> {
        Box::new(Function { variables, body })
    }
}

impl Ast for Function {
    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.body = self.body.transform(visitor);
        self
    }

    fn eval(&self, sr: &mut LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        Value::Procedure(RuntimeProcedure::new(self.body, self.variables, sr.clone()))
    }
}

#[derive(Debug)]
pub struct RegularApplication {
    function: AstNode,
    arguments: Vec<AstNode>,
}

impl RegularApplication {
    pub fn new(function: AstNode, arguments: Vec<AstNode>) -> Ref<Self> {
        Box::new(RegularApplication {
            function,
            arguments,
        })
    }
}

impl Ast for RegularApplication {
    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.function = self.function.transform(visitor);
        self.arguments = self
            .arguments
            .into_iter()
            .map(|a| a.transform(visitor))
            .collect();
        self
    }
}

#[derive(Debug)]
pub struct PredefinedApplication {
    variable: Variable,
    arguments: Vec<AstNode>,
}

impl PredefinedApplication {
    pub fn new(variable: Variable, arguments: Vec<AstNode>) -> Ref<Self> {
        Box::new(PredefinedApplication {
            variable,
            arguments,
        })
    }
}

impl Ast for PredefinedApplication {
    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.arguments = self
            .arguments
            .into_iter()
            .map(|a| a.transform(visitor))
            .collect();
        self
    }

    fn eval(&self, sr: &mut LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        let name = self.variable.name();
        let args: Vec<_> = self.arguments.iter().map(|a| a.eval(sr, sg)).collect();
        let func = sg.get_predefined(name);
        func.invoke(args)
    }
}

#[derive(Debug)]
pub struct FixLet {
    variables: Vec<Variable>,
    arguments: Vec<AstNode>,
    body: AstNode,
}

impl FixLet {
    pub fn new(variables: Vec<Variable>, arguments: Vec<AstNode>, body: AstNode) -> Ref<Self> {
        Box::new(FixLet {
            variables,
            arguments,
            body,
        })
    }
}

impl Ast for FixLet {
    fn default_transform(mut self: Ref<Self>, visitor: &mut dyn Transformer) -> AstNode {
        self.arguments = self
            .arguments
            .into_iter()
            .map(|a| a.transform(visitor))
            .collect();
        self.body = self.body.transform(visitor);
        self
    }

    fn eval(&self, sr: &mut LexicalRuntimeEnv, sg: &mut GlobalRuntimeEnv) -> Value {
        let args: Vec<_> = self.arguments
            .iter()
            .map(|x| x.eval(sr, sg))
            .collect();

        for (var, val) in self.variables.iter().zip(args) {
            sr.push_lexical(var.name(), val);
        }

        let result = self.body.eval(sr, sg);

        for _ in 0..self.variables.len() {
            sr.pop_lexical();
        }

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
        FunctionDescription {
            arity, text
        }
    }
}

#[derive(Debug)]
pub struct RuntimePrimitive {
    pub func: fn(args: Vec<Value>)->Value,
    pub arity: Arity,
}

impl RuntimePrimitive {
    pub fn new(arity: Arity, func: fn(args: Vec<Value>)->Value) -> Self {
        RuntimePrimitive {
            arity, func
        }
    }

    pub fn invoke(&self, args: Vec<Value>) -> Value {
        if self.arity.check(args.len()) {
            (self.func)(args)
        } else {
            panic!("Incorrect arity")
        }
    }
}

#[derive(Debug)]
pub struct RuntimeProcedure {
    pub body: AstNode,
    pub variables: Vec<Variable>,
    pub env: LexicalRuntimeEnv,
}

impl RuntimeProcedure {
    pub fn new(body: AstNode, variables: Vec<Variable>, env: LexicalRuntimeEnv) -> Self {
        RuntimeProcedure {
            body, variables, env
        }
    }

    pub fn invoke(&self, args: Vec<Value>) -> Value {
        unimplemented!()
        /*if self.arity.check(args.len()) {
            (self.func)(args)
        } else {
            panic!("Incorrect arity")
        }*/
    }
}