use crate::bytecode::{CodeObject, Op};
use crate::description::Arity;
use crate::env::Environment;
use crate::scm::Scm;
use crate::symbol::Symbol;
use crate::syntax::definition::GlobalDefine;
use crate::syntax::{
    Alternative, Assignment, BoxCreate, BoxRead, BoxWrite, Constant, Expression, FixLet,
    FlatClosure, FreeReference, Function, GlobalAssignment, GlobalReference, GlobalVariable,
    Import, LocalReference, PredefinedApplication, PredefinedReference, PredefinedVariable,
    Reference, RegularApplication, Sequence,
};
use crate::utils::{Named, Sourced};

#[derive(Debug)]
pub struct BytecodeGenerator {
    globals: Environment<GlobalVariable>,
    predef: Environment<PredefinedVariable>,
    constants: Vec<Scm>,
    env: Vec<Symbol>,
    current_closure_vars: Vec<Symbol>,
}

impl BytecodeGenerator {
    pub fn new(
        current_closure_vars: Vec<Symbol>,
        globals: Environment<GlobalVariable>,
        predef: Environment<PredefinedVariable>,
    ) -> Self {
        BytecodeGenerator {
            globals,
            predef,
            constants: vec![],
            env: vec![],
            current_closure_vars,
        }
    }

    pub fn compile_toplevel(
        expr: &Expression,
        globals: Environment<GlobalVariable>,
        predef: Environment<PredefinedVariable>,
    ) -> CodeObject {
        let mut bcgen = Self::new(vec![], globals, predef);
        let mut code = bcgen.compile(expr, true);
        code.push(Op::Return);
        CodeObject::new(
            Arity::Exact(0),
            expr.source().clone(),
            code,
            bcgen.constants,
        )
    }

    pub fn compile_function(
        func: &Function,
        closure_vars: Vec<Symbol>,
        globals: Environment<GlobalVariable>,
        predef: Environment<PredefinedVariable>,
    ) -> CodeObject {
        let mut bcgen = Self::new(closure_vars, globals, predef);
        bcgen.env = func.variables.iter().map(|var| var.name()).collect();
        let mut code = bcgen.compile(&func.body, true);
        if !code.last().map(Op::is_terminal).unwrap_or(false) {
            code.push(Op::Return);
        }
        CodeObject::new(func.arity(), func.span.clone(), code, bcgen.constants)
    }

    fn compile(&mut self, node: &Expression, tail: bool) -> Vec<Op> {
        use crate::syntax::Application::*;
        use Expression::*;
        let code: Vec<_> = match node {
            NoOp(_) => vec![],
            Constant(c) => self.compile_constant(c, tail),
            Sequence(s) => self.compile_sequence(s, tail),
            Alternative(a) => self.compile_alternative(a, tail),
            Reference(r) => self.compile_reference(r, tail),
            Assignment(a) => self.compile_assignment(a, tail),
            FixLet(f) => self.compile_fixlet(f, tail),
            FlatClosure(c) => self.compile_closure(c, tail),
            Application(RegularApplication(a)) => self.compile_application(a, tail),
            Application(PredefinedApplication(p)) => self.compile_predefined_application(p, tail),
            BoxCreate(b) => self.compile_box_create(b, tail),
            BoxWrite(b) => self.compile_box_write(b, tail),
            BoxRead(b) => self.compile_box_read(b, tail),
            GlobalDefine(d) => self.compile_global_def(d),
            Import(i) => self.compile_import(i),
            _ => unimplemented!(
                "Byte code compilation of:\n {:#?}\n {:?}",
                node.source(),
                node
            ),
        };

        code
    }

    fn compile_reference(&mut self, node: &Reference, tail: bool) -> Vec<Op> {
        use Reference::*;
        let code: Vec<_> = match node {
            LocalReference(l) => self.compile_local_ref(l, tail),
            FreeReference(f) => self.compile_free_ref(f, tail),
            GlobalReference(g) => self.compile_global_ref(g, tail),
            PredefinedReference(p) => self.compile_predef_ref(p, tail),
        };
        code
    }

    fn compile_assignment(&mut self, node: &Assignment, tail: bool) -> Vec<Op> {
        use Assignment::*;
        let code: Vec<_> = match node {
            LocalAssignment(_) => {
                unimplemented!("Local assignment should happen through a box write")
            }
            GlobalAssignment(g) => self.compile_global_set(g, tail),
        };
        code
    }

    fn compile_constant(&mut self, node: &Constant, _tail: bool) -> Vec<Op> {
        let value: Scm = (&node.value).into();
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

    fn compile_local_ref(&mut self, node: &LocalReference, _tail: bool) -> Vec<Op> {
        let idx = self.index_of_local(&node.var.name());
        vec![Op::LocalRef(idx)]
    }

    fn compile_free_ref(&mut self, node: &FreeReference, _tail: bool) -> Vec<Op> {
        let idx = self
            .current_closure_vars
            .iter()
            .position(|&fv| fv == node.var.name())
            .unwrap();
        vec![Op::FreeRef(idx)]
    }

    fn compile_global_ref(&mut self, node: &GlobalReference, _tail: bool) -> Vec<Op> {
        let idx = self.globals.find_idx(&node.var.name()).unwrap();
        vec![Op::GlobalRef(idx)]
    }

    fn compile_predef_ref(&mut self, node: &PredefinedReference, _tail: bool) -> Vec<Op> {
        let idx = self.predef.find_idx(&node.var.name()).unwrap();
        vec![Op::PredefRef(idx)]
    }

    fn compile_global_set(&mut self, node: &GlobalAssignment, _tail: bool) -> Vec<Op> {
        let idx = self.globals.find_idx(&node.variable.name()).unwrap();
        let mut meaning = self.compile(&node.form, false);
        meaning.push(Op::GlobalSet(idx));
        meaning
    }

    fn compile_global_def(&mut self, node: &GlobalDefine) -> Vec<Op> {
        let idx = self.globals.find_idx(&node.variable.name()).unwrap();
        let mut meaning = self.compile(&node.form, false);
        meaning.push(Op::GlobalDef(idx));
        meaning
    }

    fn compile_fixlet(&mut self, node: &FixLet, tail: bool) -> Vec<Op> {
        let n = self.env.len();

        let mut meaning = vec![];
        for (var, arg) in node.variables.iter().zip(&node.arguments) {
            let m = self.compile(arg, false);
            meaning.extend(m);
            meaning.push(Op::PushVal);
            self.env.push(var.name());
        }

        let m = self.compile(&node.body, tail);
        meaning.extend(m);

        self.env.truncate(n);
        if !meaning.last().map(Op::is_terminal).unwrap_or(false) {
            // No need to generate instructions after a terminal instruction.
            // Tail calls should truncate the value stack correctly.
            meaning.push(Op::Drop(node.variables.len()));
        }

        meaning
    }

    fn compile_closure(&mut self, node: &FlatClosure, _tail: bool) -> Vec<Op> {
        let free_vars = node.free_vars.iter().map(|s| s.var.name()).collect();

        let function = BytecodeGenerator::compile_function(
            &node.func,
            free_vars,
            self.globals.clone(),
            self.predef.clone(),
        );
        let function = Box::leak(Box::new(function));

        let mut meaning = vec![];

        for fv in node.free_vars.iter().rev() {
            let m = self.compile_local_ref(fv, false);
            meaning.extend(m);
            meaning.push(Op::PushVal);
        }

        let n_free = node.free_vars.len();
        meaning.push(Op::MakeClosure(n_free, function));

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
        _tail: bool,
    ) -> Vec<Op> {
        let mut meaning = vec![];

        for a in &node.arguments {
            let m = self.compile(a, false);
            meaning.extend(m);
            meaning.push(Op::PushVal);
        }

        let idx = self.predef.find_idx(&node.variable.name()).unwrap();
        meaning.push(Op::PredefRef(idx));

        let arity = node.arguments.len();
        meaning.push(Op::Call(arity));

        meaning
    }

    fn compile_box_create(&mut self, node: &BoxCreate, _tail: bool) -> Vec<Op> {
        let idx = self
            .env
            .iter()
            .enumerate()
            .rev()
            .find(|&(_, &v)| v == node.variable.name())
            .unwrap()
            .0;
        self.env.push(node.variable.name());
        vec![Op::Boxify(idx)]
    }

    fn compile_box_write(&mut self, node: &BoxWrite, _tail: bool) -> Vec<Op> {
        let mut meaning = self.compile_reference(&node.reference, false);
        meaning.push(Op::PushVal);
        meaning.extend(self.compile(&node.form, false));
        meaning.push(Op::BoxSet);
        meaning
    }

    fn compile_box_read(&mut self, node: &BoxRead, _tail: bool) -> Vec<Op> {
        let mut meaning = self.compile_reference(&node.reference, false);
        meaning.push(Op::BoxGet);
        meaning
    }

    fn index_of_local(&self, name: &Symbol) -> usize {
        self.env
            .iter()
            .enumerate()
            .rev()
            .find(|&(_, v)| v == name)
            .unwrap()
            .0
    }

    fn compile_import(&mut self, node: &Import) -> Vec<Op> {
        unimplemented!()
    }
}
