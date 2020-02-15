use crate::bytecode::{CodeObject, LibraryObject, Op};
use crate::error::{CompileError, Error, ErrorContext, ErrorKind, Result};
use crate::library::ExportItem;
use crate::objectify::{ObjectifyErrorKind, Translate};
use crate::primitive::{Arity, RuntimePrimitive};
use crate::scm::Scm;
use crate::symbol::Symbol;
use crate::syntax::definition::GlobalDefine;
use crate::syntax::variable::VarDef;
use crate::syntax::{
    Alternative, Assignment, BoxCreate, BoxRead, BoxWrite, Constant, Expression, FixLet,
    FlatClosure, FreeReference, Function, GlobalAssignment, GlobalReference, Import, LetContKind,
    LetContinuation, Library, LocalReference, PredefinedApplication, PredefinedReference, Program,
    Reference, RegularApplication, Sequence,
};
use crate::utils::{Named, Sourced};
use std::path::{Path, PathBuf};

pub fn compile_program(prog: &Program, trans: &Translate) -> Result<CodeObject> {
    let mut bcgen = BytecodeGenerator::new(vec![], trans);
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
    global_offset: usize,
) -> Result<CodeObject> {
    let mut bcgen = BytecodeGenerator::new(closure_vars, trans);
    bcgen.set_global_offset(global_offset);
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
    global_offset: usize,
) -> Result<LibraryObject> {
    let mut bcgen = BytecodeGenerator::new(vec![], &trans);
    bcgen.set_global_offset(global_offset);

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
pub struct BytecodeGenerator<'a> {
    trans: &'a Translate,
    constants: Vec<Scm>,
    env: Vec<Symbol>,
    current_closure_vars: Vec<Symbol>,
    libs: Vec<PathBuf>,
    imports: Vec<(usize, Symbol, usize)>,
    global_offset: usize,
}

impl<'a> BytecodeGenerator<'a> {
    pub fn new(current_closure_vars: Vec<Symbol>, trans: &'a Translate) -> Self {
        BytecodeGenerator {
            trans,
            constants: vec![],
            env: vec![],
            current_closure_vars,
            libs: vec![],
            imports: vec![],
            global_offset: 0,
        }
    }

    pub fn set_global_offset(&mut self, offset: usize) {
        self.global_offset = offset;
    }

    fn compile(&mut self, node: &Expression, tail: bool) -> Result<Vec<Op>> {
        use crate::syntax::Application::*;
        use Expression::*;
        match node {
            NoOp(_) => Ok(vec![]),
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

    fn compile_reference(&mut self, node: &Reference, tail: bool) -> Result<Vec<Op>> {
        use Reference::*;
        match node {
            LocalReference(l) => self.compile_local_ref(l, tail),
            FreeReference(f) => self.compile_free_ref(f, tail),
            GlobalReference(g) => self.compile_global_ref(g, tail),
            PredefinedReference(p) => self.compile_predef_ref(p, tail),
        }
    }

    fn compile_assignment(&mut self, node: &Assignment, tail: bool) -> Result<Vec<Op>> {
        use Assignment::*;
        match node {
            LocalAssignment(_) => {
                unimplemented!("Local assignment should happen through a box write")
            }
            GlobalAssignment(g) => self.compile_global_set(g, tail),
        }
    }

    fn compile_constant(&mut self, node: &Constant, _tail: bool) -> Result<Vec<Op>> {
        Ok(vec![self.build_constant((&node.value).into())])
    }

    fn build_constant(&mut self, value: Scm) -> Op {
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

    fn compile_local_ref(&mut self, node: &LocalReference, _tail: bool) -> Result<Vec<Op>> {
        let idx = self.index_of_local(&node.var.name());
        Ok(vec![Op::LocalRef(idx)])
    }

    fn compile_free_ref(&mut self, node: &FreeReference, _tail: bool) -> Result<Vec<Op>> {
        let idx = self
            .current_closure_vars
            .iter()
            .position(|&fv| fv == node.var.name())
            .unwrap();
        Ok(vec![Op::FreeRef(idx)])
    }

    fn compile_global_ref(&mut self, node: &GlobalReference, _tail: bool) -> Result<Vec<Op>> {
        let idx = self
            .find_global_idx(node.var.name())
            .ok_or_else(|| ObjectifyErrorKind::UndefinedVariable(node.var.name()))?;
        Ok(vec![Op::GlobalRef(idx)])
    }

    fn compile_predef_ref(&mut self, node: &PredefinedReference, _tail: bool) -> Result<Vec<Op>> {
        let idx = self.trans.env.find_predef_idx(&node.var.name()).unwrap();
        Ok(vec![Op::PredefRef(idx)])
    }

    fn compile_global_set(&mut self, node: &GlobalAssignment, _tail: bool) -> Result<Vec<Op>> {
        let idx = self.find_global_idx(node.variable.name()).unwrap();
        let mut meaning = self.compile(&node.form, false)?;
        meaning.push(Op::GlobalSet(idx));
        Ok(meaning)
    }

    fn compile_global_def(&mut self, node: &GlobalDefine) -> Result<Vec<Op>> {
        let mut meaning = self.compile(&node.form, false)?;
        meaning.push(self.build_global_def(node.variable.name()));
        Ok(meaning)
    }

    fn build_global_def(&mut self, name: Symbol) -> Op {
        let idx = self.find_global_idx(name).unwrap();
        Op::GlobalDef(idx)
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

    fn compile_closure(&mut self, node: &FlatClosure, _tail: bool) -> Result<Vec<Op>> {
        let free_vars = node.free_vars.iter().map(|s| s.var_name()).collect();

        let function = compile_function(&node.func, free_vars, self.trans, self.global_offset)?;
        let function = Box::leak(Box::new(function));

        let mut meaning = vec![];

        for fv in node.free_vars.iter().rev() {
            let m = self.compile_reference(fv, false)?;
            meaning.extend(m);
            meaning.push(Op::PushVal);
        }

        let n_free = node.free_vars.len();
        meaning.push(Op::MakeClosure(n_free, function));

        Ok(meaning)
    }

    fn compile_application(&mut self, node: &RegularApplication, tail: bool) -> Result<Vec<Op>> {
        // todo: does Scheme require that the function is evaluated first?
        let mut meaning = vec![];

        for a in &node.arguments {
            let m = self.compile(a, false)?;
            meaning.extend(m);
            meaning.push(Op::PushVal);
        }

        if let Some(mi) = self.compile_intrinsic_application(&node.function)? {
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

        Ok(meaning)
    }

    fn compile_intrinsic_application(&mut self, func: &Expression) -> Result<Option<Vec<Op>>> {
        match func {
            Expression::Reference(Reference::GlobalReference(GlobalReference { var, .. })) => {
                match var.value() {
                    VarDef::Value(Scm::Primitive(RuntimePrimitive { name, .. })) => match name {
                        "cons" => Ok(Some(vec![Op::Cons])),
                        "car" => Ok(Some(vec![Op::Car])),
                        "cdr" => Ok(Some(vec![Op::Cdr])),
                        "call/cc" => Ok(Some(vec![Op::PushCC(1), Op::Call(1)])),
                        "call/ep" => Ok(Some(vec![Op::PushEP(2), Op::Call(1), Op::PopEP])),
                        _ => Ok(None),
                    },
                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    fn compile_predefined_application(
        &mut self,
        node: &PredefinedApplication,
        _tail: bool,
    ) -> Result<Vec<Op>> {
        let mut meaning = vec![];

        for a in &node.arguments {
            let m = self.compile(a, false)?;
            meaning.extend(m);
            meaning.push(Op::PushVal);
        }

        let idx = self
            .trans
            .env
            .find_predef_idx(&node.variable.name())
            .unwrap();
        meaning.push(Op::PredefRef(idx));

        let arity = node.arguments.len();
        meaning.push(Op::Call(arity));

        Ok(meaning)
    }

    fn compile_box_create(&mut self, node: &BoxCreate, _tail: bool) -> Result<Vec<Op>> {
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

    fn compile_box_write(&mut self, node: &BoxWrite, _tail: bool) -> Result<Vec<Op>> {
        let mut meaning = self.compile_reference(&node.reference, false)?;
        meaning.push(Op::PushVal);
        meaning.extend(self.compile(&node.form, false)?);
        meaning.push(Op::BoxSet);
        Ok(meaning)
    }

    fn compile_box_read(&mut self, node: &BoxRead, _tail: bool) -> Result<Vec<Op>> {
        let mut meaning = self.compile_reference(&node.reference, false)?;
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
            LetContKind::ExitProcedure => splice!(vec![Op::PushEP(n + 1)], meaning_body, vec![Op::PopEP]),
        })
    }

    fn compile_import(&mut self, node: &Import) -> Result<Vec<Op>> {
        let mut import_ops = vec![];
        for set in &node.import_sets {
            for item in &set.items {
                if let ExportItem::Macro(_) = item.item {
                    continue;
                }
                self.add_import(
                    &set.library_path,
                    item.export_name,
                    self.find_global_idx(item.import_name).unwrap(),
                );

                let libname = set.library_path.to_str().unwrap();
                import_ops.push(self.build_constant(Scm::string(libname)));
                import_ops.push(Op::PushVal);
                import_ops.push(self.build_constant(Scm::Symbol(item.export_name)));
                import_ops.push(Op::Import);
                import_ops.push(self.build_global_def(item.import_name));
                import_ops.push(Op::Drop(1));
            }
        }

        Ok(import_ops)
    }

    fn add_import(&mut self, lib_path: &Path, export_name: Symbol, global_idx: usize) {
        let libidx = if let Some(idx) = self.libs.iter().position(|p| p == lib_path) {
            idx
        } else {
            self.libs.push(lib_path.to_owned());
            self.libs.len() - 1
        };

        self.imports.push((libidx, export_name, global_idx));
    }

    fn find_global_idx(&self, name: Symbol) -> Option<usize> {
        self.trans
            .env
            .find_global_idx(&name)
            .map(|idx| idx + self.global_offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::VirtualMachine;
    use crate::env::Env;
    use crate::source::SourceLocation::NoSource;
    use crate::syntax::GlobalVariable;

    #[test]
    fn compile_intrinsics() {
        let expr = Expression::Reference(Reference::GlobalReference(GlobalReference::new(
            GlobalVariable::defined(
                "no-matter",
                Scm::Primitive(RuntimePrimitive::new(
                    "cons",
                    Arity::Exact(2),
                    |_: &[Scm], _: &mut VirtualMachine| unimplemented!(),
                )),
            ),
            NoSource,
        )));

        let trans = Translate::new(Env::new());
        let mut gen = BytecodeGenerator::new(vec![], &trans);
        let code = gen.compile_intrinsic_application(&expr).unwrap();
        assert_eq!(code, Some(vec![Op::Cons]));
    }
}
