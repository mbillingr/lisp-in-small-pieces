use crate::ast::{
    Alternative, AstNode, Constant, FixLet, Function, LocalReference, Ref, Sequence, Transformer,
    Variable, Visited,
};
use crate::ast_transform::flatten_closures::FlatClosure;
use crate::bytecode::{CodeObject, Op};
use crate::env::{GlobalRuntimeEnv, LexicalRuntimeEnv};
use crate::scm::Scm;
use crate::source::SourceLocation;
use crate::value::Value;

#[derive(Debug)]
pub struct BytecodeGenerator {
    constants: Vec<Scm>,
    env: Vec<Variable>,
}

impl BytecodeGenerator {
    pub fn new() -> Self {
        BytecodeGenerator {
            constants: vec![],
            env: vec![],
        }
    }

    pub fn compile_toplevel(node: &AstNode) -> CodeObject {
        let mut bcgen = Self::new();
        let mut code = bcgen.compile(node, true);
        code.push(Op::Return);
        CodeObject::new(code, bcgen.constants)
    }

    pub fn compile_function(func: &Function) -> CodeObject {
        let mut bcgen = Self::new();
        let mut code = bcgen.compile(&func.body, true);
        code.push(Op::Return);
        CodeObject::new(code, bcgen.constants)
    }

    fn compile(&mut self, node: &AstNode, tail: bool) -> Vec<Op> {
        let code: Vec<_> = dispatch! { self on node:
            c as Constant => self.compile_constant(c, tail),
            s as Sequence => self.compile_sequence(s, tail),
            a as Alternative => self.compile_alternative(a, tail),
            r as LocalReference => self.compile_local_ref(r, tail),
            f as FixLet => self.compile_fixlet(f, tail),
            c as FlatClosure => self.compile_closure(c, tail),
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
        m1.push(Op::Drop);
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
            .find(|&(_, v)| v == node.variable())
            .unwrap()
            .0;
        vec![Op::LocalRef(idx)]
    }

    fn compile_fixlet(&mut self, node: &FixLet, tail: bool) -> Vec<Op> {
        let n = self.env.len();

        let mut meaning = vec![];
        for (var, arg) in node.variables.iter().zip(&node.arguments) {
            let m = self.compile(arg, false);
            meaning.extend(m);
            self.env.push(var.clone());
        }

        let m = self.compile(&node.body, tail);
        meaning.extend(m);

        self.env.truncate(n);

        meaning
    }

    fn compile_closure(&mut self, node: &FlatClosure, tail: bool) -> Vec<Op> {
        let function = BytecodeGenerator::compile_function(&node.func);
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
}
