use crate::env::Env;
use crate::error::{Error, Result};
use crate::library::{is_import, libname_to_path};
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::source::SourceLocation::NoSource;
use crate::source::{Source, SourceLocation};
use crate::symbol::Symbol;
use crate::syntax::definition::GlobalDefine;
use crate::syntax::variable::VarDef;
use crate::syntax::{
    Alternative, Application, Expression, FixLet, Function, GlobalAssignment, GlobalReference,
    GlobalVariable, Import, ImportItem, ImportSet, Library, LibraryDeclaration, LibraryExport,
    LocalAssignment, LocalReference, LocalVariable, MagicKeyword, NoOp, Program, Reference,
    Sequence, Variable,
};
use crate::utils::{Named, Sourced};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::path::{Path, PathBuf};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum ObjectifyErrorKind {
    SyntaxError(&'static str),
    NoPair,
    IncorrectArity,
    ImmutableAssignment,
    ExpectedList,
    ExpectedSymbol,
    UnknownLibrary(PathBuf),
    InvalidLibraryDefinition,
    UndefinedVariable(Symbol),
    MismatchedEllipses,
}

#[derive(Debug)]
pub struct Translate {
    pub current_lib: Symbol,
    pub env: Env,
    pub base_env: Env,
    pub libs: Rc<RefCell<HashMap<PathBuf, Rc<Library>>>>,
}

impl Translate {
    pub fn new(base_env: Env) -> Self {
        Translate {
            current_lib: Symbol::new("/"),
            env: base_env.clone(),
            base_env,
            libs: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn same_but_empty(&self) -> Self {
        Translate {
            current_lib: self.current_lib,
            env: Env::new(),
            base_env: Env::new(),
            libs: self.libs.clone(),
        }
    }

    /// Mark the current environment as this translation's base environment
    pub fn mark_base_env(&mut self) {
        self.base_env = self.env.clone();
    }

    pub fn objectify_toplevel(&mut self, exprs: &[TrackedSexpr]) -> Result<Program> {
        self.objectify_program(exprs)
    }

    pub fn objectify_program(&mut self, exprs: &[TrackedSexpr]) -> Result<Program> {
        let n_imports = exprs.iter().take_while(|&expr| is_import(expr)).count();

        let imports: Import = exprs[..n_imports]
            .iter()
            .rev()
            .map(|expr| self.objectify_import(expr))
            .collect::<Result<_>>()?;

        let mut sequence = TrackedSexpr::nil(NoSource);
        for expr in exprs[n_imports..].iter().rev() {
            sequence = TrackedSexpr::cons(expr.clone(), sequence, NoSource);
        }

        let body = if sequence.is_null() {
            Expression::NoOp(NoOp)
        } else {
            /*sequence =
                TrackedSexpr::cons(TrackedSexpr::symbol("begin", NoSource), sequence, NoSource);
            self.objectify(&sequence)?*/
            self.objectify_sequence(&sequence)?
        };

        let span = exprs.last().unwrap().source().start_at(exprs[0].source());

        Ok(Program::new(imports, body, span))
    }

    pub fn objectify(&mut self, expr: &TrackedSexpr) -> Result<Expression> {
        match &expr.sexpr {
            Sexpr::SyntacticClosure(sc) => sc.expand(self),
            _ if expr.is_symbol() => self.objectify_symbol(expr),
            _ if expr.is_atom() => self.objectify_quotation(expr),
            _ => {
                let m = self.objectify(ocar(expr)?)?;
                if let Expression::MagicKeyword(MagicKeyword { name: _, handler }) = m {
                    handler.invoke(self, expr)
                } else {
                    self.objectify_application(&m, ocdr(expr)?, expr.source().clone())
                }
            }
        }
    }

    pub fn objectify_quotation(&mut self, expr: &TrackedSexpr) -> Result<Expression> {
        Ok(Expression::Constant(expr.clone().into()))
    }

    pub fn objectify_alternative(
        &mut self,
        condition: &TrackedSexpr,
        consequence: &TrackedSexpr,
        alternative: Option<&TrackedSexpr>,
        span: SourceLocation,
    ) -> Result<Expression> {
        let condition = self.objectify(condition)?;
        let consequence = self.objectify(consequence)?;
        let alternative = match alternative {
            Some(alt) => self.objectify(alt)?,
            None => Expression::Constant(TrackedSexpr::undefined().into()),
        };
        Ok(Alternative::new(condition, consequence, alternative, span).into())
    }

    pub fn objectify_sequence(&mut self, exprs: &TrackedSexpr) -> Result<Expression> {
        if exprs.is_pair() {
            let car = exprs.car().unwrap();
            let cdr = exprs.cdr().unwrap();
            if cdr.is_pair() {
                let this = self.objectify(car)?;
                let next = self.objectify_sequence(cdr)?;
                Ok(Sequence::new(this, next, exprs.src.clone()).into())
            } else {
                self.objectify(car)
            }
        } else {
            Err(Error::at_expr(ObjectifyErrorKind::ExpectedList, exprs))
        }
    }

    fn objectify_symbol(&mut self, expr: &TrackedSexpr) -> Result<Expression> {
        let var_name = TrackedSexpr::as_symbol(expr).unwrap();
        //println!("{} -> {:?} in {:?}", var_name, self.env.find_variable(var_name), self.env);
        match self.env.find_variable(var_name) {
            Some(Variable::LocalVariable(v)) => {
                Ok(LocalReference::new(v, expr.source().clone()).into())
            }
            Some(Variable::GlobalVariable(v)) => match v.value() {
                _ => Ok(GlobalReference::new(v, expr.source().clone()).into()),
            },
            Some(Variable::MagicKeyword(mkw)) => Ok((mkw).into()),
            Some(Variable::FreeVariable(_)) => {
                panic!("There should be no free variables in the compile-time environment")
            }
            None => self.objectify_free_reference(var_name.clone(), expr.source().clone()),
        }
    }

    fn objectify_free_reference(
        &mut self,
        name: Symbol,
        span: SourceLocation,
    ) -> Result<Expression> {
        let v = self.adjoin_global_variable(name, VarDef::Undefined);
        Ok(GlobalReference::new(v, span).into())
    }

    fn adjoin_global_variable(&mut self, name: Symbol, def: VarDef) -> GlobalVariable {
        let v = GlobalVariable::new(self.current_lib, name, def);
        self.env.push_global(v.clone());
        v
    }

    fn objectify_application(
        &mut self,
        func: &Expression,
        mut args: &TrackedSexpr,
        span: SourceLocation,
    ) -> Result<Expression> {
        let mut args_list = vec![];
        while !args.is_null() {
            let car = args
                .car()
                .map_err(|_| Error::at_expr(ObjectifyErrorKind::ExpectedList, args))?;
            args_list.push(self.objectify(car)?);
            args = args.cdr().unwrap();
        }

        match func {
            Expression::Function(f) => self.process_closed_application(f.clone(), args_list, span),
            _ => Ok(Application::new(func.clone(), args_list, span).into()),
        }
    }

    fn process_closed_application(
        &mut self,
        func: Function,
        args: Vec<Expression>,
        span: SourceLocation,
    ) -> Result<Expression> {
        if func
            .variables
            .last()
            .map(LocalVariable::is_dotted)
            .unwrap_or(false)
        {
            self.process_nary_closed_application(func, args, span)
        } else {
            if args.len() == func.variables.len() {
                Ok(FixLet::new(func.variables, args, func.body, span).into())
            } else {
                Err(Error::at_span(ObjectifyErrorKind::IncorrectArity, span))
            }
        }
    }

    fn process_nary_closed_application(
        &mut self,
        func: Function,
        mut args: Vec<Expression>,
        span: SourceLocation,
    ) -> Result<Expression> {
        let variables = func.variables;
        let body = func.body;

        if args.len() + 1 < variables.len() {
            return Err(Error::at_span(ObjectifyErrorKind::IncorrectArity, span));
        }

        let cons_var: GlobalVariable = self
            .library("sunny/core")
            .expect("The sunny/core library must be available during compilation")
            .env
            .find_predef("sunny/core", "cons")
            .expect("The cons pritimive must be present in the sunny/core library")
            .clone()
            .try_into()
            .unwrap();

        let mut dotted = Expression::Constant(TrackedSexpr::nil(span.last_char()).into());

        while args.len() >= variables.len() {
            let x = args.pop().unwrap();

            let partial_span = span.clone().start_at(x.source());

            dotted = Application::new(
                Box::new(GlobalReference::new(cons_var.clone(), NoSource).into()),
                vec![x, dotted],
                partial_span,
            )
            .into();
        }

        args.push(dotted.into());

        variables.last().unwrap().set_dotted(false);

        Ok(FixLet::new(variables, args, body, span).into())
    }

    pub fn objectify_function(
        &mut self,
        names: &TrackedSexpr,
        body: &TrackedSexpr,
        span: SourceLocation,
    ) -> Result<Expression> {
        let vars = self.objectify_variables_list(names)?;
        self.env.extend_local(vars.iter().cloned());
        let bdy = self.objectify_sequence(body)?;
        self.env.drop_frame(vars.len());
        Ok(Function::new(vars, bdy, span).into())
    }

    fn objectify_variables_list(&mut self, mut names: &TrackedSexpr) -> Result<Vec<LocalVariable>> {
        let mut vars = vec![];
        while let Ok(car) = names.car() {
            let name = car.as_symbol()?;
            vars.push(LocalVariable::new(*name, false, false));

            names = names.cdr().unwrap();
        }

        if !names.is_null() {
            let name = names.as_symbol()?;
            vars.push(LocalVariable::new(*name, false, true));
        }

        Ok(vars)
    }

    pub fn objectify_definition(
        &mut self,
        variable: &TrackedSexpr,
        expr: &TrackedSexpr,
        span: SourceLocation,
    ) -> Result<Expression> {
        let form = self.objectify(expr)?;

        let var_name = TrackedSexpr::as_symbol(variable).unwrap();
        let gvar = match self.env.find_variable(var_name) {
            Some(Variable::LocalVariable(_)) => panic!("untransformed local define"),
            Some(Variable::GlobalVariable(v)) => {
                v.set_mutable(true);
                v.set_value(VarDef::Unknown);
                v
            }
            _ => {
                let v = self.adjoin_global_variable(*var_name, VarDef::Unknown);
                match &form {
                    Expression::Constant(c) => v.set_value(VarDef::Value((&c.value).into())),
                    Expression::Reference(Reference::GlobalReference(gr)) => {
                        v.set_value(gr.var.value())
                    }
                    _ => v.set_value(VarDef::Unknown),
                }
                v
            }
        };

        Ok(GlobalDefine::new(gvar, form, span).into())
    }

    pub fn objectify_assignment(
        &mut self,
        variable: &TrackedSexpr,
        expr: &TrackedSexpr,
        span: SourceLocation,
    ) -> Result<Expression> {
        let ov = self.objectify_symbol(variable)?;
        let of = self.objectify(expr)?;

        match ov {
            Expression::Reference(Reference::LocalReference(r)) => {
                r.var.set_mutable(true);
                Ok(LocalAssignment::new(r, of, span).into())
            }
            Expression::Reference(Reference::GlobalReference(GlobalReference { var, .. })) => {
                let gvar = var;

                gvar.set_mutable(true);
                gvar.set_value(VarDef::Unknown);

                Ok(GlobalAssignment::new(gvar, of, span).into())
            }
            _ => Err(Error::at_span(
                ObjectifyErrorKind::ImmutableAssignment,
                span,
            )),
        }
    }

    fn objectify_import(&mut self, expr: &TrackedSexpr) -> Result<Import> {
        let mut import_sets = expr.cdr()?;

        let mut sets = vec![];

        while import_sets.is_pair() {
            let import_obj = self.objectify_import_set(import_sets.car().unwrap())?;

            let mut import_vars = vec![];
            let mut import_macros = vec![];
            for item in &import_obj.items {
                match &item.import_var {
                    Variable::GlobalVariable(gv) => import_vars.push(gv.clone()),
                    Variable::MagicKeyword(_) => import_macros.push(item.import_var.clone()),
                    _ => panic!("invalid import"),
                }
            }

            for var in import_vars {
                let gv = self.env.ensure_global(var.clone());
                gv.ensure_value(var.value());
            }

            self.env.extend_global(import_macros);

            sets.push(import_obj);
            import_sets = import_sets.cdr().unwrap();
        }

        Ok(Import::new(sets, expr.source().clone()))
    }

    fn objectify_import_set(&mut self, form: &TrackedSexpr) -> Result<ImportSet> {
        let modifier = form.car()?.as_symbol();
        match modifier {
            Ok(s) if s == "only" => self.objectify_import_only(form),
            Ok(s) if s == "except" => self.objectify_import_except(form),
            Ok(s) if s == "prefix" => self.objectify_import_prefix(form),
            Ok(s) if s == "rename" => self.objectify_import_rename(form),
            _ => self.objectify_import_all(form),
        }
    }

    fn objectify_import_all(&mut self, form: &TrackedSexpr) -> Result<ImportSet> {
        let library_path = libname_to_path(form)?;

        let mut libtrans = Translate::new(Env::new());
        libtrans.libs = self.libs.clone();

        let lib = libtrans.load_library(form)?;

        Ok(ImportSet::new(
            form.into(),
            library_path,
            lib.exports
                .iter()
                .map(|spec| {
                    let export_var = lib.lookup(spec);
                    let import_var: Variable = match &export_var {
                        Variable::GlobalVariable(ex) => GlobalVariable::constant(
                            self.current_lib,
                            spec.exported_name(),
                            ex.value(),
                        )
                        .into(),
                        Variable::MagicKeyword(mkw) => MagicKeyword {
                            name: spec.exported_name(),
                            handler: mkw.handler.clone(),
                        }
                        .into(),
                        _ => panic!("invalid import"),
                    };
                    ImportItem::new(export_var, import_var)
                })
                .collect(),
            form.source().clone(),
        ))
    }

    fn objectify_import_only(&mut self, form: &TrackedSexpr) -> Result<ImportSet> {
        let import_set = form.cdr().unwrap().car().unwrap();
        let mut identifiers = form.cdr().unwrap().cdr().unwrap();

        let mut import_set = self.objectify_import_set(import_set)?;

        let mut only_names = HashSet::new();

        while identifiers.is_pair() {
            let identifier = *identifiers.car().unwrap().as_symbol().unwrap();
            only_names.insert(identifier);
            identifiers = identifiers.cdr().unwrap();
        }
        import_set.items = import_set
            .items
            .into_iter()
            .filter(|item| only_names.remove(&item.import_var.name()))
            .collect();

        if only_names.is_empty() {
            Ok(import_set)
        } else {
            Err(
                ObjectifyErrorKind::UndefinedVariable(only_names.into_iter().next().unwrap())
                    .into(),
            )
        }
    }

    fn objectify_import_except(&mut self, form: &TrackedSexpr) -> Result<ImportSet> {
        let import_set = form.cdr().unwrap().car().unwrap();
        let mut identifiers = form.cdr().unwrap().cdr().unwrap();

        let mut import_set = self.objectify_import_set(import_set)?;

        while identifiers.is_pair() {
            let identifier = *identifiers.car().unwrap().as_symbol().unwrap();
            import_set.items = import_set
                .items
                .into_iter()
                .filter(|item| item.import_var.name() != identifier)
                .collect();
            identifiers = identifiers.cdr().unwrap();
        }

        Ok(import_set)
    }

    fn objectify_import_prefix(&mut self, form: &TrackedSexpr) -> Result<ImportSet> {
        let import_set = form.cdr().unwrap().car().unwrap();
        let prefix = *form
            .cdr()
            .unwrap()
            .cdr()
            .unwrap()
            .car()
            .unwrap()
            .as_symbol()
            .unwrap();

        let mut import_set = self.objectify_import_set(import_set)?;

        import_set.items = import_set
            .items
            .into_iter()
            .map(|item| ImportItem {
                import_var: item.import_var.renamed(prefix + item.import_var.name()),
                ..item
            })
            .collect();

        Ok(import_set)
    }

    fn objectify_import_rename(&mut self, form: &TrackedSexpr) -> Result<ImportSet> {
        let import_set = form.cdr().unwrap().car().unwrap();
        let mut mappings = form.cdr().unwrap().cdr().unwrap();

        let mut import_set = self.objectify_import_set(import_set)?;

        let mut mapping = HashMap::new();
        while mappings.is_pair() {
            let original = mappings.car().unwrap().car().unwrap().as_symbol().unwrap();
            let newname = mappings
                .car()
                .unwrap()
                .cdr()
                .unwrap()
                .car()
                .unwrap()
                .as_symbol()
                .unwrap();
            mapping.insert(*original, *newname);
            mappings = mappings.cdr().unwrap();
        }

        import_set.items = import_set
            .items
            .into_iter()
            .map(|item| ImportItem {
                import_var: if let Some(&newname) = mapping.get(&item.import_var.name()) {
                    item.import_var.renamed(newname)
                } else {
                    item.import_var
                },
                ..item
            })
            .collect();

        Ok(import_set)
    }

    pub fn objectify_library(
        &mut self,
        name: &TrackedSexpr,
        body: &TrackedSexpr,
        span: SourceLocation,
    ) -> Result<Library> {
        let prev_lib = self.current_lib;
        self.current_lib = Symbol::from_str(libname_to_path(name)?.to_str().unwrap());

        let lib = self.objectify_library_declarations(body)?;

        let mut imports: Option<Import> = None;
        let mut exports = LibraryExport::new(NoSource);
        let mut body = Expression::NoOp(NoOp);

        for decl in lib {
            match decl {
                LibraryDeclaration::Import(import) => {
                    imports = if let Some(i) = imports {
                        Some(i.join(import))
                    } else {
                        Some(import)
                    };
                }
                LibraryDeclaration::LibraryExport(x) => exports.extend(x),
                LibraryDeclaration::Expression(x) => body = body.splice(x),
            }
        }

        let imports = imports.unwrap_or(Import::new(vec![], NoSource));

        self.current_lib = prev_lib;

        Ok(Library::new(self.env.clone(), imports, exports, body, span))
    }

    pub fn objectify_library_declarations(
        &mut self,
        body: &TrackedSexpr,
    ) -> Result<Vec<LibraryDeclaration>> {
        if body.is_pair() {
            let car = body.car().unwrap();
            let cdr = body.cdr().unwrap();
            if cdr.is_pair() {
                let mut declarations = vec![self.objectify_library_declaration(car)?];
                declarations.extend(self.objectify_library_declarations(cdr)?);
                Ok(declarations)
            } else {
                Ok(vec![self.objectify_library_declaration(car)?])
            }
        } else {
            Err(Error::at_expr(ObjectifyErrorKind::ExpectedList, body))
        }
    }

    pub fn objectify_library_declaration(
        &mut self,
        decl: &TrackedSexpr,
    ) -> Result<LibraryDeclaration> {
        match decl.car() {
            Ok(x) if x == "begin" => self.objectify_sequence(decl.cdr().unwrap()).map(Into::into),
            Ok(x) if x == "export" => self
                .objectify_library_export(decl.cdr().unwrap())
                .map(Into::into),
            Ok(x) if x == "import" => self.objectify_library_import(decl).map(Into::into),
            _ => Err(Error::at_expr(
                ObjectifyErrorKind::InvalidLibraryDefinition,
                decl,
            )),
        }
    }

    pub fn objectify_library_import(&mut self, decl: &TrackedSexpr) -> Result<Import> {
        self.objectify_import(decl)
    }

    pub fn objectify_library_export(&mut self, specs: &TrackedSexpr) -> Result<LibraryExport> {
        match specs.sexpr {
            Sexpr::Nil => Ok(LibraryExport::new(specs.source().last_char())),
            Sexpr::Pair(_) => {
                let car = specs.car().unwrap();
                let cdr = specs.cdr().unwrap();

                let mut export = self.objectify_library_export(cdr)?;
                match car.sexpr {
                    Sexpr::Symbol(ident) => export.adjoin_identifier(ident, car.source()),
                    Sexpr::Pair(_) if car.car().unwrap() == "rename" => {
                        let (old, new) = car
                            .cdr()
                            .and_then(|cdar| cdar.car().map(|cadar| (cadar, cdar.cdr().unwrap())))
                            .and_then(|(cadar, cddar)| cddar.car().map(|caddar| (cadar, caddar)))
                            .and_then(|(cadar, caddar)| {
                                (cadar.as_symbol().map(|old| (old, caddar)))
                            })
                            .and_then(|(old, caddar)| {
                                (caddar.as_symbol().map(|new| (*old, *new)))
                            })?;
                        export.adjoin_rename(old, new, car.source());
                    }
                    _ => Err(Error::at_expr(
                        ObjectifyErrorKind::InvalidLibraryDefinition,
                        car,
                    ))?,
                };
                Ok(export)
            }
            _ => Err(Error::at_expr(
                ObjectifyErrorKind::InvalidLibraryDefinition,
                specs,
            )),
        }
    }

    fn load_library(&mut self, library_name: &TrackedSexpr) -> Result<Rc<Library>> {
        let library_path = libname_to_path(library_name)?;
        if self.libs.borrow().contains_key(&library_path) {
            Ok(self.libs.borrow()[&library_path].clone())
        } else {
            let mut file_path = library_path.clone();
            file_path.set_extension("sld");

            let library_code = Source::from_file(&file_path).map_err(|err| match err.kind() {
                std::io::ErrorKind::NotFound => {
                    Error::at_expr(ObjectifyErrorKind::UnknownLibrary(file_path), library_name)
                }
                _ => err.into(),
            })?;

            let lib = self.parse_library(library_code)?;
            let lib = Rc::new(lib);

            self.libs
                .borrow_mut()
                .insert(library_path.clone(), lib.clone());
            Ok(lib)
        }
    }

    pub fn parse_library(&mut self, code: Source) -> Result<Library> {
        let sexprs = TrackedSexpr::from_source(&code)?;
        if sexprs.len() == 0 {
            return Err(Error::at_span(
                ObjectifyErrorKind::InvalidLibraryDefinition,
                code.into(),
            ));
        }
        if ocar(&sexprs[0])? != "define-library" {
            return Err(Error::at_span(
                ObjectifyErrorKind::InvalidLibraryDefinition,
                sexprs[0].source().clone(),
            ));
        }
        if sexprs.len() > 1 {
            let span = sexprs.last().unwrap().source().start_at(sexprs[1].source());
            return Err(Error::at_span(
                ObjectifyErrorKind::InvalidLibraryDefinition,
                span,
            ));
        }

        let sexpr = &sexprs[0];
        let name = ocar(ocdr(sexpr)?)?;
        let body = ocdr(ocdr(sexpr)?)?;
        self.objectify_library(name, body, sexpr.source().clone())
    }

    pub fn add_library(&mut self, library_name: impl Into<PathBuf>, library: Library) {
        self.libs
            .borrow_mut()
            .insert(library_name.into(), Rc::new(library));
    }

    pub fn library(&mut self, libname: &str) -> Option<Rc<Library>> {
        let path = Path::new(libname);
        self.libs.borrow().get(path).cloned()
    }
}

pub fn ocar(e: &TrackedSexpr) -> Result<&TrackedSexpr> {
    e.car()
}

pub fn ocdr(e: &TrackedSexpr) -> Result<&TrackedSexpr> {
    e.cdr()
}

pub fn decons(e: &TrackedSexpr) -> Result<(&TrackedSexpr, &TrackedSexpr)> {
    ocar(e).and_then(|car| ocdr(e).map(|cdr| (car, cdr)))
}
