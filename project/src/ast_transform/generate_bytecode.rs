use crate::ast::{
    Alternative, Ast, AstNode, Constant, FixLet, Function, GlobalAssignment, GlobalReference,
    LocalReference, PredefinedApplication, PredefinedReference, Ref, RegularApplication, Sequence,
    Transformer, Variable, Visited,
};
use crate::ast_transform::flatten_closures::FlatClosure;
use crate::bytecode::{CodeObject, Op};
use crate::env::{Env, Environment, GlobalRuntimeEnv, LexicalRuntimeEnv};
use crate::scm::Scm;
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use crate::value::Value;
use std::rc::Rc;

#[derive(Debug)]
pub struct BytecodeGenerator {
    globals: Environment,
    predef: Environment,
    constants: Vec<Scm>,
    env: Vec<Symbol>,
}

impl BytecodeGenerator {
    pub fn new(globals: Environment, predef: Environment) -> Self {
        BytecodeGenerator {
            globals,
            predef,
            constants: vec![],
            env: vec![],
        }
    }

    pub fn compile_toplevel(
        node: &AstNode,
        globals: Environment,
        predef: Environment,
    ) -> CodeObject {
        let mut bcgen = Self::new(globals, predef);
        let mut code = bcgen.compile(node, true);
        code.push(Op::Return);
        CodeObject::new(node.source().clone(), code, bcgen.constants)
    }

    pub fn compile_function(
        func: &Function,
        globals: Environment,
        predef: Environment,
    ) -> CodeObject {
        let mut bcgen = Self::new(globals, predef);
        bcgen.env = func.variables.iter().map(Variable::name).copied().collect();
        let mut code = bcgen.compile(&func.body, true);
        code.push(Op::Return);
        CodeObject::new(func.source().clone(), code, bcgen.constants)
    }

    fn compile(&mut self, node: &AstNode, tail: bool) -> Vec<Op> {
        let code: Vec<_> = dispatch! { self on node:
            c as Constant => self.compile_constant(c, tail),
            s as Sequence => self.compile_sequence(s, tail),
            a as Alternative => self.compile_alternative(a, tail),
            r as LocalReference => self.compile_local_ref(r, tail),
            r as GlobalReference => self.compile_global_ref(r, tail),
            r as PredefinedReference => self.compile_predef_ref(r, tail),
            r as GlobalAssignment => self.compile_global_set(r, tail),
            f as FixLet => self.compile_fixlet(f, tail),
            c as FlatClosure => self.compile_closure(c, tail),
            a as RegularApplication => self.compile_application(a, tail),
            p as PredefinedApplication => self.compile_predefined_application(p, tail),
            _ => { unimplemented!("Byte code compilation of:\n {:#?}\n", node.source()) }
        };

        code
    }

    fn compile_constant(&mut self, node: &Constant, _tail: bool) -> Vec<Op> {
        let value: Scm = node.value.clone().into();
        let idx = self.constants.iter().position(|x| x.equals(&value));
        let idx = match idx {
            None => {
                let n = self.constants.len();
                self.constants.push(value);
                n
            }
            Some(i) => i,
        };
        match node.value {
            _ => vec![Op::Constant(idx)],
        }
    }

    fn compile_sequence(&mut self, node: &Sequence, tail: bool) -> Vec<Op> {
        let mut m1 = self.compile(&node.first, false);
        let m2 = self.compile(&node.next, tail);
        m1.extend(m2);
        m1
    }

    fn compile_alternative(&mut self, node: &Alternative, tail: bool) -> Vec<Op> {
        let m1 = self.compile(&node.condition, false);
        let m2 = self.compile(&node.consequence, tail);
        let m3 = self.compile(&node.alternative, tail);

        let mut meaning = m1;
        meaning.push(Op::JumpFalse(m2.len() as isize + 1));
        meaning.extend(m2);
        meaning.push(Op::Jump(m3.len() as isize));
        meaning.extend(m3);
        meaning
    }

    fn compile_local_ref(&mut self, node: &LocalReference, tail: bool) -> Vec<Op> {
        let idx = self
            .env
            .iter()
            .enumerate()
            .rev()
            .find(|&(_, v)| v == node.variable().name())
            .unwrap()
            .0;
        vec![Op::LocalRef(idx)]
    }

    fn compile_global_ref(&mut self, node: &GlobalReference, tail: bool) -> Vec<Op> {
        let idx = self.globals.find_idx(node.var.name()).unwrap();
        vec![Op::GlobalRef(idx)]
    }

    fn compile_predef_ref(&mut self, node: &PredefinedReference, tail: bool) -> Vec<Op> {
        let idx = self.predef.find_idx(node.var.name()).unwrap();
        vec![Op::PredefRef(idx)]
    }

    fn compile_global_set(&mut self, node: &GlobalAssignment, tail: bool) -> Vec<Op> {
        let idx = self.globals.find_idx(node.variable.name()).unwrap();
        let mut meaning = self.compile(&node.form, false);
        meaning.push(Op::GlobalSet(idx));
        meaning
    }

    fn compile_fixlet(&mut self, node: &FixLet, tail: bool) -> Vec<Op> {
        let n = self.env.len();

        let mut meaning = vec![];
        for (var, arg) in node.variables.iter().zip(&node.arguments) {
            let m = self.compile(arg, false);
            meaning.extend(m);
            meaning.push(Op::PushVal);
            self.env.push(*var.name());
        }

        let m = self.compile(&node.body, tail);
        meaning.extend(m);

        self.env.truncate(n);
        meaning.push(Op::Drop(node.variables.len()));

        meaning
    }

    fn compile_closure(&mut self, node: &FlatClosure, tail: bool) -> Vec<Op> {
        let function = BytecodeGenerator::compile_function(
            &node.func,
            self.globals.clone(),
            self.predef.clone(),
        );
        let function = Box::leak(Box::new(function));

        let mut meaning = vec![];

        for fv in node.free_vars.iter().rev() {
            let m = self.compile(fv, false);
            meaning.extend(m);
        }

        let n_free = node.free_vars.len();
        meaning.push(Op::MakeClosure(function, n_free));

        meaning
    }

    fn compile_application(&mut self, node: &RegularApplication, tail: bool) -> Vec<Op> {
        // todo: does Scheme require that the function is evaluated first?
        let mut meaning = vec![];

        for a in &node.arguments {
            let m = self.compile(a, false);
            meaning.extend(m);
            meaning.push(Op::PushVal);
        }

        let mf = self.compile(&node.function, false);
        meaning.extend(mf);

        let arity = node.arguments.len();

        match tail {
            true => meaning.push(Op::TailCall(arity)),
            false => meaning.push(Op::Call(arity)),
        }

        meaning
    }

    fn compile_predefined_application(
        &mut self,
        node: &PredefinedApplication,
        tail: bool,
    ) -> Vec<Op> {
        let mut meaning = vec![];

        for a in &node.arguments {
            let m = self.compile(a, false);
            meaning.extend(m);
            meaning.push(Op::PushVal);
        }

        let idx = self.predef.find_idx(node.variable.name()).unwrap();
        meaning.push(Op::PredefRef(idx));

        let arity = node.arguments.len();
        meaning.push(Op::Call(arity));

        meaning
    }
}
