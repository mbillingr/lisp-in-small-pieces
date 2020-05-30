use crate::bytecode::Op;
use crate::error::{CompileError, Error, ErrorContext, ErrorKind, Result};
use crate::interpreter::{encode_arity, CodeObject, LibraryObject};
use crate::objectify::Translate;
use crate::scm::Scm;
use crate::syntax::definition::GlobalDefine;
use crate::syntax::{
    Alternative, Application, Assignment, BoxCreate, BoxRead, BoxWrite, Constant, Expression,
    FixLet, FlatClosure, FreeReference, Function, GlobalAssignment, GlobalReference,
    GlobalVariable, Import, LetContKind, LetContinuation, Library, LocalReference, Program,
    Reference, Sequence, Variable,
};
use std::collections::HashMap;
use std::path::PathBuf;
use sunny_common::{Arity, Named, Sourced, Symbol};

pub fn compile_program(
    prog: &Program,
    trans: &Translate,
    glob_alloc: &mut GlobalAllocator,
) -> Result<CodeObject> {
    let mut bcgen = BytecodeGenerator::new(vec![], trans, glob_alloc);
    let mut code = vec![];
    code.extend(bcgen.compile_import(&prog.imports)?);
    code.extend(bcgen.compile(&prog.body, true)?);
    code.push(Op::Return);

    let mut library_code = vec![];
    for lib in bcgen.libs.clone() {
        library_code.push(bcgen.build_constant(Scm::string(lib.to_str().unwrap())));
        library_code.push(Op::InitLibrary);
    }

    library_code.extend(code);

    Ok(CodeObject::new(
        Arity::Exact(0),
        prog.body.source().clone(),
        library_code,
        bcgen.constants,
    ))
}

pub fn compile_function(
    func: &Function,
    closure_vars: Vec<Symbol>,
    trans: &Translate,
    glob_alloc: &mut GlobalAllocator,
) -> Result<CodeObject> {
    let mut bcgen = BytecodeGenerator::new(closure_vars, trans, glob_alloc);
    bcgen.env = func.variables.iter().map(|var| var.name()).collect();
    let mut code = bcgen.compile(&func.body, true)?;
    code.push(Op::Return);
    Ok(CodeObject::new(
        func.arity(),
        func.span.clone(),
        code,
        bcgen.constants,
    ))
}

pub fn compile_library(
    lib: &Library,
    trans: &Translate,
    glob_alloc: &mut GlobalAllocator,
) -> Result<LibraryObject> {
    let mut bcgen = BytecodeGenerator::new(vec![], &trans, glob_alloc);

    let mut code = vec![];
    code.extend(bcgen.compile_import(&lib.imports)?);
    code.extend(bcgen.compile(&lib.body, false)?);
    code.push(Op::Return);

    let mut library_code = vec![];
    for lib in bcgen.libs.clone() {
        library_code.push(bcgen.build_constant(Scm::string(lib.to_str().unwrap())));
        library_code.push(Op::InitLibrary);
    }

    library_code.extend(code);

    let global_symbols: Vec<_> = trans.env.globals().map(|gv| gv.name()).collect();

    Ok(LibraryObject::new(
        lib.source().clone(),
        library_code,
        bcgen.constants,
        global_symbols,
        lib.exports.clone(),
    ))
}

#[derive(Debug)]
pub struct GlobalAllocator {
    globals: HashMap<GlobalVariable, usize>,
    vars: Vec<GlobalVariable>,
}

impl GlobalAllocator {
    pub fn new() -> Self {
        GlobalAllocator {
            globals: HashMap::new(),
            vars: vec![],
        }
    }

    pub fn get_idx(&mut self, var: &GlobalVariable) -> usize {
        if let Some(idx) = self.idx(var) {
            idx
        } else {
            self.new_idx(var)
        }
    }

    pub fn new_idx(&mut self, var: &GlobalVariable) -> usize {
        //println!("new index: {:?}", var.full_name());
        let idx = self.vars.len();
        self.vars.push(var.clone());
        self.globals.insert(var.clone(), idx);
        idx
    }

    pub fn idx(&mut self, var: &GlobalVariable) -> Option<usize> {
        self.globals.get(var).copied()
    }

    pub fn declare_alias(&mut self, var: &GlobalVariable, alias: &GlobalVariable) -> usize {
        match (self.idx(var), self.idx(alias)) {
            (None, None) => {
                println!("new-new: {:?} {:?}", var, alias);
                let idx = self.new_idx(var);
                self.globals.insert(alias.clone(), idx);
                idx
            }
            (Some(idx), None) => {
                //println!("old-new: {:?} {:?}", var, alias);
                self.globals.insert(alias.clone(), idx);
                idx
            }
            (None, Some(idx)) => {
                println!("new-old: {:?} {:?}", var, alias);
                self.globals.insert(var.clone(), idx);
                self.vars[idx] = var.clone();
                idx
            }
            (Some(idx1), Some(idx2)) => {
                if idx1 != idx2 {
                    println!(
                        "WARNING: existing global alias:    {:?} -> {:?}",
                        var.full_name(),
                        alias.full_name()
                    );
                    self.globals.insert(alias.clone(), idx1);
                }
                idx2
            }
        }
    }

    pub fn n_vars(&self) -> usize {
        self.vars.len()
    }

    pub fn find_var(&self, idx: usize) -> &GlobalVariable {
        &self.vars[idx]
    }
}

#[derive(Debug)]
pub struct BytecodeGenerator<'a> {
    trans: &'a Translate,
    constants: Vec<Scm>,
    env: Vec<Symbol>,
    current_closure_vars: Vec<Symbol>,
    libs: Vec<PathBuf>,
    glob_alloc: &'a mut GlobalAllocator,
}

impl<'a> BytecodeGenerator<'a> {
    pub fn new(
        current_closure_vars: Vec<Symbol>,
        trans: &'a Translate,
        glob_alloc: &'a mut GlobalAllocator,
    ) -> Self {
        BytecodeGenerator {
            trans,
            constants: vec![],
            env: vec![],
            current_closure_vars,
            libs: vec![],
            glob_alloc,
        }
    }

    fn compile(&mut self, node: &Expression, tail: bool) -> Result<Vec<Op>> {
        use Expression::*;
        match node {
            NoOp(_) => Ok(vec![]),
            Constant(c) => self.compile_constant(c),
            Sequence(s) => self.compile_sequence(s, tail),
            Alternative(a) => self.compile_alternative(a, tail),
            Reference(r) => self.compile_reference(r),
            Assignment(a) => self.compile_assignment(a),
            FixLet(f) => self.compile_fixlet(f, tail),
            FlatClosure(c) => self.compile_closure(c),
            Application(a) => self.compile_application(a, tail),
            BoxCreate(b) => self.compile_box_create(b),
            BoxWrite(b) => self.compile_box_write(b),
            BoxRead(b) => self.compile_box_read(b),
            GlobalDefine(d) => self.compile_global_def(d),
            MagicKeyword(m) => Err(Error {
                kind: ErrorKind::Compile(CompileError::MacroUsedAsValue(m.name)),
                context: ErrorContext::Source(node.source().clone()),
            }),
            LetContinuation(l) => self.compile_letcont(l, tail),
            _ => unimplemented!(
                "Byte code compilation of:\n {:#?}\n {:?}",
                node.source(),
                node
            ),
        }
    }

    fn compile_reference(&mut self, node: &Reference) -> Result<Vec<Op>> {
        use Reference::*;
        match node {
            LocalReference(l) => self.compile_local_ref(l),
            FreeReference(f) => self.compile_free_ref(f),
            GlobalReference(g) => self.compile_global_ref(g),
        }
    }

    fn compile_assignment(&mut self, node: &Assignment) -> Result<Vec<Op>> {
        use Assignment::*;
        match node {
            LocalAssignment(_) => {
                unimplemented!("Local assignment should happen through a box write")
            }
            GlobalAssignment(g) => self.compile_global_set(g),
        }
    }

    fn compile_constant(&mut self, node: &Constant) -> Result<Vec<Op>> {
        Ok(vec![self.build_constant(&node.value)])
    }

    fn push_constant(&mut self, value: impl Into<Scm>) -> Vec<Op> {
        vec![self.build_constant(value), Op::PushVal]
    }

    fn build_constant(&mut self, value: impl Into<Scm>) -> Op {
        let value = value.into();
        let idx = self.constants.iter().position(|x| x.equals(&value));
        let idx = match idx {
            None => {
                let n = self.constants.len();
                self.constants.push(value);
                n
            }
            Some(i) => i,
        };
        Op::Constant(idx)
    }

    fn compile_sequence(&mut self, node: &Sequence, tail: bool) -> Result<Vec<Op>> {
        let mut m1 = self.compile(&node.first, false)?;
        let m2 = self.compile(&node.next, tail)?;
        m1.extend(m2);
        Ok(m1)
    }

    fn compile_alternative(&mut self, node: &Alternative, tail: bool) -> Result<Vec<Op>> {
        let m1 = self.compile(&node.condition, false)?;
        let m2 = self.compile(&node.consequence, tail)?;
        let m3 = self.compile(&node.alternative, tail)?;

        let mut meaning = m1;
        meaning.push(Op::JumpFalse(m2.len() as isize + 1));
        meaning.extend(m2);
        meaning.push(Op::Jump(m3.len() as isize));
        meaning.extend(m3);
        Ok(meaning)
    }

    fn compile_local_ref(&mut self, node: &LocalReference) -> Result<Vec<Op>> {
        let idx = self.index_of_local(&node.var.name());
        Ok(vec![Op::LocalRef(idx)])
    }

    fn compile_free_ref(&mut self, node: &FreeReference) -> Result<Vec<Op>> {
        let idx = self
            .current_closure_vars
            .iter()
            .position(|&fv| fv == node.var.name())
            .unwrap();
        Ok(vec![Op::FreeRef(idx)])
    }

    fn compile_global_ref(&mut self, node: &GlobalReference) -> Result<Vec<Op>> {
        let idx = self.glob_alloc.get_idx(&node.var);
        Ok(vec![Op::GlobalRef(idx)])
    }

    fn compile_global_set(&mut self, node: &GlobalAssignment) -> Result<Vec<Op>> {
        let idx = self.glob_alloc.get_idx(&node.variable);
        let mut meaning = self.compile(&node.form, false)?;
        meaning.push(Op::GlobalSet(idx));
        Ok(meaning)
    }

    fn compile_global_def(&mut self, node: &GlobalDefine) -> Result<Vec<Op>> {
        let mut meaning = self.compile(&node.form, false)?;
        meaning.push(if node.redefine {
            let idx = self.glob_alloc.new_idx(&node.variable);
            Op::GlobalDef(idx)
        } else {
            let idx = self.glob_alloc.get_idx(&node.variable);
            Op::GlobalDef(idx)
        });
        Ok(meaning)
    }

    fn compile_fixlet(&mut self, node: &FixLet, tail: bool) -> Result<Vec<Op>> {
        let n = self.env.len();

        let mut meaning = vec![];
        for (var, arg) in node.variables.iter().zip(&node.arguments) {
            let m = self.compile(arg, false)?;
            meaning.extend(m);
            meaning.push(Op::PushVal);
            self.env.push(var.name());
        }

        let m = self.compile(&node.body, tail)?;
        meaning.extend(m);

        self.env.truncate(n);
        if !meaning.last().map(Op::is_terminal).unwrap_or(false) {
            // No need to generate instructions after a terminal instruction.
            // Tail calls should truncate the value stack correctly.
            meaning.push(Op::Drop(node.variables.len()));
        }

        Ok(meaning)
    }

    fn compile_closure(&mut self, node: &FlatClosure) -> Result<Vec<Op>> {
        let free_vars = node.free_vars.iter().map(|s| s.var_name()).collect();

        let function = compile_function(&node.func, free_vars, self.trans, self.glob_alloc)?;

        let mut meaning = vec![];

        for fv in node.free_vars.iter().rev() {
            let m = self.compile_reference(fv)?;
            meaning.extend(m);
            meaning.push(Op::PushVal);
        }

        let n_free = node.free_vars.len();

        let offset = self.push_constant(1);
        let arity = self.push_constant(encode_arity(function.arity));
        meaning.extend(offset);
        meaning.extend(arity);

        let n_constants = function.constants.len();
        for c in function.constants.iter().rev() {
            meaning.extend(self.push_constant(c));
        }
        meaning.extend(self.push_constant(n_constants));

        meaning.push(Op::MakeClosure(n_free));
        meaning.push(Op::Jump(function.ops.len() as isize));
        meaning.extend(function.ops);

        Ok(meaning)
    }

    fn compile_application(&mut self, node: &Application, tail: bool) -> Result<Vec<Op>> {
        // todo: does Scheme require that the function is evaluated first?
        let mut meaning = vec![];

        let n = self.env.len();

        for a in &node.arguments {
            let m = self.compile(a, false)?;
            meaning.extend(m);
            meaning.push(Op::PushVal);
            self.env.push(Symbol::uninterned("_")); // add dummy variable to env, so that other variables are indexed correctly
        }

        if let Some(mi) =
            self.compile_intrinsic_application(&node.function, node.arguments.len(), tail)?
        {
            if !meaning.is_empty() {
                meaning.pop(); // don't push last argument
            }
            meaning.extend(mi);
        } else {
            let mf = self.compile(&node.function, false)?;
            meaning.extend(mf);

            let arity = node.arguments.len();

            match tail {
                true => meaning.push(Op::TailCall(arity)),
                false => meaning.push(Op::Call(arity)),
            }
        }

        self.env.truncate(n);

        Ok(meaning)
    }

    fn compile_intrinsic_application(
        &mut self,
        func: &Expression,
        n_args: usize,
        tail: bool,
    ) -> Result<Option<Vec<Op>>> {
        match func {
            Expression::Reference(Reference::GlobalReference(GlobalReference { var, .. })) => {
                if let Some(obj) = var.object() {
                    match obj.id() {
                        "sunny/core/cons" => Ok(Some(vec![Op::Cons])),
                        "sunny/core/car" => Ok(Some(vec![Op::Car])),
                        "sunny/core/cdr" => Ok(Some(vec![Op::Cdr])),
                        "sunny/core/call/cc" => Ok(Some(vec![Op::PushCC(1), Op::Call(1)])),
                        "sunny/core/call/ep" => {
                            Ok(Some(vec![Op::PushEP(2), Op::Call(1), Op::PopEP]))
                        }
                        "sunny/core/apply" => Ok(Some(if tail {
                            vec![Op::PreApply(n_args), Op::TailCallN]
                        } else {
                            vec![Op::PreApply(n_args), Op::CallN, Op::Drop(1)]
                        })),
                        _ => Ok(None),
                    }
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }

    fn compile_box_create(&mut self, node: &BoxCreate) -> Result<Vec<Op>> {
        let idx = self
            .env
            .iter()
            .enumerate()
            .rev()
            .find(|&(_, &v)| v == node.variable.name())
            .unwrap()
            .0;
        self.env.push(node.variable.name());
        Ok(vec![Op::Boxify(idx)])
    }

    fn compile_box_write(&mut self, node: &BoxWrite) -> Result<Vec<Op>> {
        let mut meaning = self.compile_reference(&node.reference)?;
        meaning.push(Op::PushVal);
        meaning.extend(self.compile(&node.form, false)?);
        meaning.push(Op::BoxSet);
        Ok(meaning)
    }

    fn compile_box_read(&mut self, node: &BoxRead) -> Result<Vec<Op>> {
        let mut meaning = self.compile_reference(&node.reference)?;
        meaning.push(Op::BoxGet);
        Ok(meaning)
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

    fn compile_letcont(&mut self, node: &LetContinuation, tail: bool) -> Result<Vec<Op>> {
        self.env.push(node.variable.name());

        let mut meaning_body = self.compile(&node.body, tail)?;

        self.env.pop();

        if !meaning_body.last().map(Op::is_terminal).unwrap_or(false) {
            // No need to generate instructions after a terminal instruction.
            // Tail calls should truncate the value stack correctly.
            meaning_body.push(Op::Drop(1));
        }

        let n = meaning_body.len() as isize;

        Ok(match node.kind {
            LetContKind::IndefiniteContinuation => splice!(vec![Op::PushCC(n)], meaning_body),
            LetContKind::ExitProcedure => {
                splice!(vec![Op::PushEP(n + 1)], meaning_body, vec![Op::PopEP])
            }
        })
    }

    fn compile_import(&mut self, node: &Import) -> Result<Vec<Op>> {
        for set in &node.import_sets {
            for item in &set.items {
                match (&item.export_var, &item.import_var) {
                    (Variable::MagicKeyword(_), _) => continue,
                    (Variable::GlobalVariable(ex), Variable::GlobalVariable(im)) => {
                        self.glob_alloc.declare_alias(ex, im);
                    }
                    _ => panic!("Invalid Import"),
                }
            }

            if !self.libs.contains(&set.library_path) {
                self.libs.push(set.library_path.clone())
            }
        }

        Ok(vec![])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Env;
    use crate::syntax::variable::GlobalObject;
    use crate::syntax::GlobalVariable;
    use sunny_common::SourceLocation::NoSource;

    #[test]
    fn compile_intrinsics() {
        let expr = Expression::Reference(Reference::GlobalReference(GlobalReference::new(
            GlobalVariable::constant(
                Symbol::new(""),
                "no-matter",
                GlobalObject::new("sunny/core", "cons"),
                /*Scm::Primitive(RuntimePrimitive::new(
                    "cons",
                    Arity::Exact(2),
                    |_: &[Scm], _: &mut VirtualMachine| unimplemented!(),
                )),*/
            ),
            NoSource,
        )));

        let mut ga = GlobalAllocator::new();

        let trans = Translate::new(Env::new());
        let mut gen = BytecodeGenerator::new(vec![], &trans, &mut ga);
        let code = gen.compile_intrinsic_application(&expr, 2, true).unwrap();
        assert_eq!(code, Some(vec![Op::Cons]));
    }
}
