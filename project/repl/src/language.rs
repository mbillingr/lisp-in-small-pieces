pub mod scheme {
    use crate::ast_transform::boxify::Boxify;
    use crate::ast_transform::flatten_closures::Flatten;
    use crate::ast_transform::generate_bytecode::compile_program;
    use crate::bytecode::{Closure, VirtualMachine};
    use crate::env::Env;
    use crate::error::{Error, Result, RuntimeError, TypeError};
    use crate::library::{LibraryBuilder, LibraryData};
    use crate::macro_language::eval_syntax;
    use crate::objectify::{decons, ocar, ocdr, ObjectifyErrorKind, Translate};
    use crate::ports::SchemePort;
    use crate::primitive::{Arity, RuntimePrimitive};
    use crate::scan_out_defines::{
        definition_value, definition_variable, make_function, scan_out_defines,
    };
    use crate::scm::{ResultWrap, Scm};
    use crate::sexpr::TrackedSexpr;
    use crate::source::Source;
    use crate::source::SourceLocation::NoSource;
    use crate::syntax::{
        Expression, LetContKind, LetContinuation, Library, LocalVariable, MagicKeyword, NoOp,
    };
    use crate::utils::find_library;
    use std::collections::VecDeque;
    use std::convert::TryInto;
    use std::ops::{Div, Rem};
    use std::path::{Path, PathBuf};
    use std::time::Instant;
    use sunny_common::Symbol;

    pub struct Context {
        pub vm: VirtualMachine,
    }

    impl Context {
        pub fn new() -> Self {
            let env = Env::new();

            let trans = Translate::new(env);

            let vm = VirtualMachine::new(trans);

            Context { vm }
        }

        pub fn trans(&mut self) -> &mut Translate {
            &mut self.vm.trans
        }

        pub fn eval_str(&mut self, src: &str) -> Result<Scm> {
            self.eval_source(&src.into())
        }

        pub fn eval_source(&mut self, src: &Source) -> Result<Scm> {
            let sexpr = TrackedSexpr::from_source(src)?;
            self.eval_sexpr(&sexpr)
        }

        pub fn eval_sexpr(&mut self, sexprs: &[TrackedSexpr]) -> Result<Scm> {
            //println!("{:?} =>", sexprs);
            //println!("{:?}", self.trans);
            let ast = self.trans().objectify_toplevel(sexprs)?;
            //println!("{:?}", ast.reify());
            let ast = ast.transform(&mut Boxify);
            let ast = ast.transform(&mut Flatten::new());

            //println!("{:#?}", ast);

            for set in &ast.imports.import_sets {
                self.vm.init_library(set.library_symb.as_str())?;
            }

            let code = compile_program(&ast, &self.vm.trans, &mut self.vm.ga)?;
            //println!("{:#?}", code);
            let code = Box::leak(Box::new(code));
            let closure = Box::leak(Box::new(Closure::simple(code)));

            self.vm.synchronize_globals();
            let result = self.vm.eval(closure, &[])?;
            debug_assert!(self.vm.value_stack.is_empty());
            debug_assert!(self.vm.call_stack.is_empty());
            Ok(result)
        }

        pub fn add_library(&mut self, library_name: impl Into<PathBuf>, lib: LibraryData) {
            let name = library_name.into();
            let name_str = Scm::string(name.to_str().unwrap()).as_string().unwrap();

            let library = Library::construct(Symbol::from(name_str), lib.exports);

            self.vm.add_library(name_str, &library);

            self.trans().add_library(name.clone(), library);
        }

        pub fn import_library(&mut self, library_name: impl Into<PathBuf>) {
            let name = library_name.into();
            let parts: Vec<_> = name.iter().map(|s| s.to_str().unwrap()).collect();
            let parts = parts.join(" ");
            let expr = format!("(import ({}))", parts);
            self.eval_str(&expr).unwrap();
        }
    }

    pub fn load_library(trans: &mut Translate, path: &Path) -> Result<Library> {
        let file_path = if let Some(p) = find_library(path) {
            p
        } else {
            return Err(Error::at_span(
                ObjectifyErrorKind::UnknownLibrary(path.to_owned()),
                NoSource,
            ));
        };

        let library_src = Source::from_file(&file_path)?;

        let lib = trans.parse_library(library_src)?;
        let lib = lib.transform(&mut Boxify);
        let lib = lib.transform(&mut Flatten::new());
        Ok(lib)
    }

    pub fn create_scheme_extra_library() -> LibraryData {
        build_library! {
            native "primitive?", =1, Scm::is_primitive;
            native "uninitialized?", =1, Scm::is_uninitialized;
            native "undefined?", =1, Scm::is_undefined;

            native "disassemble", =1, disassemble;
            primitive "globals", =0, list_globals;
            primitive "timeit", =1, timeit;
        }
    }

    pub fn create_scheme_base_library() -> LibraryData {
        build_library! {
            // example of a variable-argument native
            //native "foo", >=1, |a: Scm, b: &[Scm]| {println!("{:?} and {:?}", a, b)};

            // example of a primitive function (that works on the current context)
            //primitive "globals", =0, list_globals;

            native "boolean?", =1, Scm::is_bool;
            native "null?", =1, Scm::is_nil;
            native "pair?", =1, Scm::is_pair;
            native "vector?", =1, Scm::is_vector;
            native "procedure?", =1, Scm::is_procedure;
            native "number?", =1, Scm::is_number;

            native "cons", =2, Scm::cons;
            native "car", =1, Scm::car;
            native "cdr", =1, Scm::cdr;
            native "set-car!", =2, Scm::set_car;
            native "set-cdr!", =2, Scm::set_cdr;
            native "eq?", =2, Scm::ptr_eq;
            native "eqv?", =2, Scm::ptr_eq;
            native "equal?", =2, Scm::equals;
            native "=", =2, Scm::num_eq;
            native "<", =2, Scm::num_less;
            native "*", >=0, mul;
            native "/", =2, Scm::div;
            native "%", =2, Scm::rem;
            native "+", >=0, add;
            native "-", >=0, sub;
            native "list", >=0, list;
            native "vector", >=0, vector;
            native "make-vector", >=1, make_vector;

            native "exact", =1, Scm::exact;
            native "round", =1, Scm::round;
            native "floor", =1, Scm::floor;
            native "sqrt", =1, Scm::sqrt;
            native "sin", =1, Scm::sin;
            native "cos", =1, Scm::cos;
            native "tan", =1, Scm::tan;
            native "expt", =2, Scm::pow;

            native "number->string", >=1, |z: Scm, args: &[Scm]| -> Result<Scm> {
                // quick and dirty implementation that only works for integers and panics uncontrollably...
                let radix = args.get(0).unwrap_or(&Scm::Int(10)).as_int()? as u32;
                let mut z = z.as_int()? as u32;
                let mut chars = VecDeque::new();
                loop {
                    let remainder = z % radix;
                    z /= radix;
                    chars.push_front(std::char::from_digit(remainder, radix).unwrap());
                    if z == 0 {
                        break
                    }
                }
                Ok(Scm::string(chars.into_iter().collect::<String>()))
            };

            native "string->symbol", =1, |s: Scm| s.as_string().map(|s| Scm::Symbol(Symbol::new(s)));

            native "string-append", >=0, |args: &[Scm]| args.iter().map(Scm::as_string).collect::<Result<String>>().map(Scm::string);
            native "string-length", =1, |s: &str| s.len();
            native "string-ref", =2, |s: &str, i: usize| -> Result<char> { s.chars().nth(i).ok_or(RuntimeError::IndexOutOfRange(i as isize).into()) };
            native "string-set!", =3, |string: Scm, i: usize, ch: char| -> Result<()> {
                let s = string.as_string()?;
                if i >= s.len() {
                    return Err(RuntimeError::IndexOutOfRange(i as isize).into());
                }
                let mut chars = s.chars();
                let mut newstr = String::with_capacity(s.len());
                for _ in 0..i {
                    newstr.push(chars.next().unwrap());
                }
                newstr.push(ch);
                newstr.extend(chars.skip(1));
                string.replace_string(newstr)?;
                Ok(())
            };

            native "vector-length", =1, |vec: Scm| vec.as_vector().map(|v| v.len());
            native "vector-ref", =2, Scm::vector_ref;
            native "vector-set!", =3, Scm::vector_set;
            native "vector-fill!", >=2, vector_fill;

            native "apply", >=2, |_f: Scm, _a: Scm, _args: &[Scm]| -> () { unimplemented!() };

            primitive "push-exception-handler", =1, |args: &[Scm], vm: &mut VirtualMachine| vm.push_handler(args[0]);
            primitive "pop-exception-handler", =0, |_: &[Scm], vm: &mut VirtualMachine| vm.pop_handler();
            primitive "current-exception-handler", =0, |_: &[Scm], vm: &mut VirtualMachine| vm.exception_handler;
            primitive "set-current-exception-handler!", =1, |args: &[Scm], vm: &mut VirtualMachine| vm.exception_handler = args[0];
            primitive "raise", =1, |args: &[Scm], vm: &mut VirtualMachine| vm.raise(args[0]);
            primitive "raise-continuable", =1, |args: &[Scm], vm: &mut VirtualMachine| vm.raise_continuable(args[0]);

            native "file-exists?", =1, |filename: &str| std::path::Path::new(filename).exists();
            native "delete-file", =1, |filename: &str| std::fs::remove_file(filename).map_err(Error::from);

            macro "define-syntax", expand_define_syntax;
            macro "let-syntax", expand_let_syntax;
            macro "letrec-syntax", expand_letrec_syntax;
            macro "begin", expand_begin;
            macro "define", expand_define;
            macro "if", expand_alternative;
            macro "lambda", expand_lambda;
            macro "let", expand_let;
            macro "quote", expand_quote;
            macro "set!", expand_assign;
            macro "let/cc", expand_letcc;
            macro "let/ep", expand_letep;
        }
    }

    pub fn create_scheme_ports_library() -> LibraryData {
        build_library! {
            native "eof-object?", =1, Scm::is_eof;
            native "eof-object", =0, || Scm::Eof;
            native "input-port?", =1, |port: Scm| -> Result<bool> { port.as_port().map(SchemePort::is_input_port) };
            native "output-port?", =1, |port: Scm| -> Result<bool> { port.as_port().map(SchemePort::is_output_port) };
            native "port?", =1, |port: Scm| { port.as_port().is_ok() };

            native "port-open?", =1, |port: Scm| -> Result<bool> { port.as_port().map(SchemePort::is_open) };
            native "close-port", =1, |port: Scm| -> Result<()> { port.as_port()?.close() };

            native "open-standard-input", =0, || -> Scm { SchemePort::std_input().into() };
            native "open-standard-output", =0, || -> Scm { SchemePort::std_output().into() };
            native "open-standard-error", =0, || -> Scm { SchemePort::std_error().into() };

            native "open-input-file", =1, |name: Scm| -> Result<Scm> { Ok(SchemePort::file_input(name.as_string()?)?.into()) };
            native "open-output-file", =1, |name: Scm| -> Result<Scm> { Ok(SchemePort::file_output(name.as_string()?)?.into()) };
            native "open-input-string", =1, |s: Scm| -> Result<Scm> { Ok(SchemePort::string_input(s.as_string()?).into()) };
            native "open-output-string", =0, || -> Result<Scm> { Ok(SchemePort::bytes_output().into()) };
            native "get-output-string", =1, |port: Scm| -> Result<Scm> { Ok(String::from_utf8(port.as_port()?.clone_data()?)?.into()) };
            native "open-input-bytevector", =1, |s: Scm| -> Result<Scm> { Ok(SchemePort::bytes_input(s.as_bytevec()?).into()) };
            native "open-output-bytevector", =0, || -> Result<Scm> { Ok(SchemePort::bytes_output().into()) };
            native "get-output-bytevector", =1, |port: Scm| -> Result<Scm> { Ok(port.as_port()?.clone_data()?.into()) };

            native "read", =1, |port: Scm| -> Result<Scm> { port.as_port()?.read() };
            native "read-char", =1, |port: Scm| -> Result<Scm> { port.as_port()?.read_char() };
            native "peek-char", =1, |port: Scm| -> Result<Scm> { port.as_port()?.peek_char() };
            native "read-line", =1, |port: Scm| -> Result<Scm> { port.as_port()?.read_line() };
            native "read-string", =2, |k: usize, port: Scm| -> Result<Scm> { port.as_port()?.read_string(k) };
            native "read-u8", =1, |port: Scm| -> Result<Scm> { port.as_port()?.read_u8() };
            native "peek-u8", =1, |port: Scm| -> Result<Scm> { port.as_port()?.peek_u8() };
            native "read-bytevector", =2, |k: usize, port: Scm| -> Result<Scm> { port.as_port()?.read_bytevector(k) };
            native "read-bytevector!", >=2, |vec: Scm, port: Scm, args: &[Scm]| -> Result<Scm> {
                let buf = vec.as_mut_bytevec()?;
                let (start, end) = match args {
                    [] => (0, buf.len()),
                    [s] => (s.try_into()?, buf.len()),
                    [s, e] => (s.try_into()?, e.try_into()?),
                    _ => return Err(RuntimeError::IncorrectArity.into())
                };
                port.as_port()?.read_into_bytevector(start, end, buf)
            };

            native "write", =2, |obj: Scm, port: Scm| -> Result<()> { port.as_port()?.write(obj) };
            native "write-shared", =2, |obj: Scm, port: Scm| -> Result<()> { port.as_port()?.write_shared(obj) };
            native "write-simple", =2, |obj: Scm, port: Scm| -> Result<()> { port.as_port()?.write_simple(obj) };
            native "display", =2, |obj: Scm, port: Scm| -> Result<()> { port.as_port()?.display(obj) };
            native "newline", =1, |port: Scm| -> Result<()> { port.as_port()?.write_char('\n') };
            native "write-char", =2, |ch: Scm, port: Scm| -> Result<()> { port.as_port()?.write_char(ch.as_char()?) };
            native "write-string", >=2, |s: Scm, port: Scm, args: &[Scm]| -> Result<()> {
                let buf = s.as_string()?;
                let (start, end) = match args {
                    [] => (0, buf.chars().count()),
                    [s] => (s.try_into()?, buf.chars().count()),
                    [s, e] => (s.try_into()?, e.try_into()?),
                    _ => return Err(RuntimeError::IncorrectArity.into())
                };
                port.as_port()?.write_string(buf, start, end)
            };
            native "write-u8", =2, |x: Scm, port: Scm| -> Result<()> {
                let x = x.as_int()?;
                if x >= 0 && x <= 255 {
                    port.as_port()?.write_u8(x as u8)
                } else {
                    Err(TypeError::NoU8.into())
                }
             };
            native "write-bytevector", >=2, |s: Scm, port: Scm, args: &[Scm]| -> Result<()> {
                let buf = s.as_bytevec()?;
                let (start, end) = match args {
                    [] => (0, buf.len()),
                    [s] => (s.try_into()?, buf.len()),
                    [s, e] => (s.try_into()?, e.try_into()?),
                    _ => return Err(RuntimeError::IncorrectArity.into())
                };
                port.as_port()?.write_bytevector(buf, start, end)
            };
            native "flush-output-port", =1, |port: Scm| -> Result<()> { port.as_port()?.flush_output() };
        }
    }

    // I'll leave this in, commented out, as an example how to use Syntactic closures natively.
    /*pub fn expand_or(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        let exp1 = ocar(ocdr(expr)?)?.clone();
        let exp2 = ocar(ocdr(ocdr(expr)?)?)?.clone();

        let exp1 = SyntacticClosure::new(exp1, trans.env.clone()).into();
        let exp2 = SyntacticClosure::new(exp2, trans.env.clone()).into();

        let tmp = TrackedSexpr::symbol("temp", NoSource);
        let defs = TrackedSexpr::list(
            vec![TrackedSexpr::list(vec![tmp.clone(), exp1], NoSource)],
            NoSource,
        );
        let cnd = TrackedSexpr::list(
            vec![TrackedSexpr::symbol("if", NoSource), tmp.clone(), tmp, exp2],
            NoSource,
        );

        let x = TrackedSexpr::list(
            vec![TrackedSexpr::symbol("let", NoSource), defs, cnd],
            NoSource,
        );
        let x = SyntacticClosure::new(x, trans.base_env.clone()).into();

        trans.objectify(&x)
    }*/

    pub fn expand_lambda(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        let def = &ocdr(expr)?;
        let names = ocar(def)?;

        let mut rebound = vec![];
        names.scan_improper(|name, _is_last_cdr| -> Result<()> {
            if let Some(alias) = name.as_alias() {
                rebound.push((alias.clone(), alias.alias_name().unwrap(), alias.is_bound()));
                alias.set_bound(true);
                alias.rename();
            }
            Ok(())
        })?;

        let body = ocdr(def)?;
        let body = scan_out_defines(body.clone())?;

        let result = trans.objectify_function(names, &body, expr.source().clone())?;

        for (alias, old_name, bound) in rebound {
            alias.set_bound(bound);
            alias.set_name(old_name);
        }

        Ok(result)
    }

    pub fn expand_let(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        let def = ocdr(expr)?;
        let vars = ocar(def)?;
        let body = ocdr(def)?;

        let mut var_names = vec![];
        let mut var_forms = vec![];
        vars.scan(|v| {
            v.at(0)
                .and_then(|name| v.at(1).map(|form| (name, form)))
                .map_err(|_| {
                    Error::at_expr(
                        ObjectifyErrorKind::SyntaxError("invalid variable definition in let form"),
                        body,
                    )
                })
                .map(|(name, form)| {
                    var_names.push(name.clone());
                    var_forms.push(form.clone());
                })
        })?;

        let mut rebound = vec![];
        for name in &mut var_names {
            if let Some(alias) = name.as_alias() {
                rebound.push((alias.clone(), alias.alias_name().unwrap(), alias.is_bound()));
                alias.set_bound(true);
                alias.rename();
            }
        }

        let param_list = TrackedSexpr::list(var_names, vars.src.clone());
        let func = make_function(param_list, body.clone(), expr.src.clone());

        let mut call = vec![func];
        call.extend(var_forms);

        let new_expr = TrackedSexpr::list(call, expr.src.clone());
        let result = trans.objectify(&new_expr)?;

        for (alias, old_name, bound) in rebound {
            alias.set_bound(bound);
            alias.set_name(old_name);
        }

        Ok(result)
    }

    pub fn expand_begin(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        trans.objectify_sequence(ocdr(expr)?)
    }

    pub fn expand_assign(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        expr.at(1)
            .and_then(|variable| expr.at(2).map(|expr| (variable, expr)))
            .map_err(|_| Error::at_expr(ObjectifyErrorKind::ExpectedList, expr))
            .and_then(|(variable, form)| {
                trans.objectify_assignment(variable, form, expr.source().clone())
            })
    }

    pub fn expand_quote(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        let body = &ocdr(expr)?;
        trans.objectify_quotation(ocar(body)?)
    }

    pub fn expand_alternative(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        let rest = ocdr(expr)?;
        let (cond, rest) = decons(&rest)?;
        let (yes, rest) = decons(&rest)?;
        let no = decons(&rest).ok().map(|(no, _)| no);
        trans.objectify_alternative(cond, yes, no, expr.source().clone())
    }

    pub fn expand_define(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        let definee = definition_variable(expr)?;
        let form = definition_value(expr)?;
        trans.objectify_definition(definee, &form, expr.source().clone())
    }

    pub fn expand_letcc(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        expand_letcont(trans, expr, LetContKind::IndefiniteContinuation)
    }

    pub fn expand_letep(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        expand_letcont(trans, expr, LetContKind::ExitProcedure)
    }

    pub fn expand_letcont(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        kind: LetContKind,
    ) -> Result<Expression> {
        let def = ocdr(expr)?;
        let var_name = ocar(def)?;
        let body = ocdr(def)?;

        let mut rebound = None;
        if let Some(alias) = var_name.as_alias() {
            rebound = Some((alias.clone(), alias.alias_name().unwrap(), alias.is_bound()));
            alias.set_bound(true);
            alias.rename();
        }

        let var = LocalVariable::new(var_name.as_symbol()?, false, false);

        trans.env.push_local(var.clone());
        let body = trans.objectify_sequence(body)?;
        trans.env.drop_frame(1);

        let ast = LetContinuation::new(kind, var, body, expr.source().clone());

        for (alias, old_name, bound) in rebound {
            alias.set_bound(bound);
            alias.set_name(old_name);
        }

        Ok(ast.into())
    }

    pub fn expand_define_syntax(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        let name = *expr.at(1)?.as_symbol()?;
        let handler = eval_syntax(expr.cdr()?, &trans.env)?;
        let macro_binding = MagicKeyword { name, handler };
        trans.env.push_global(macro_binding);
        Ok(Expression::NoOp(NoOp))
    }

    pub fn expand_let_syntax(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        let def = ocdr(expr)?;
        let bindings = ocar(def)?;
        let body = ocdr(def)?;

        let mut rebound = vec![];
        let mut macros = vec![];
        bindings.scan(|v| -> Result<()> {
            let name = v.car()?;
            if let Some(alias) = name.as_alias() {
                rebound.push((alias.clone(), alias.alias_name().unwrap(), alias.is_bound()));
                alias.set_bound(true);
                alias.rename();
            }

            let name = *name.as_symbol()?;
            let handler = eval_syntax(v, &trans.env)?;
            let macro_binding = MagicKeyword { name, handler };
            macros.push(macro_binding);
            Ok(())
        })?;

        let n_macros = macros.len();
        trans.env.extend_local(macros);

        let result = trans.objectify_sequence(body);

        trans.env.drop_frame(n_macros);

        for (alias, old_name, bound) in rebound {
            alias.set_bound(bound);
            alias.set_name(old_name);
        }

        result
    }

    pub fn expand_letrec_syntax(trans: &mut Translate, expr: &TrackedSexpr) -> Result<Expression> {
        let def = ocdr(expr)?;
        let bindings = ocar(def)?;
        let body = ocdr(def)?;

        let mut rebound = vec![];
        let mut macros = vec![];
        bindings.scan(|v| -> Result<()> {
            let name = v.car()?;
            if let Some(alias) = name.as_alias() {
                rebound.push((alias.clone(), alias.alias_name().unwrap(), alias.is_bound()));
                alias.set_bound(true);
                alias.rename();
            }

            let name = *name.as_symbol()?;
            let macro_binding = MagicKeyword::new(name, |_, _| unimplemented!());
            trans.env.push_local(macro_binding.clone());
            macros.push(macro_binding);
            Ok(())
        })?;

        let n_macros = macros.len();
        let mut macros = macros.into_iter();

        bindings.scan(|v| -> Result<()> {
            let handler = eval_syntax(v, &trans.env)?;
            let macro_binding = macros.next().unwrap();
            unsafe {
                macro_binding.replace_handler(handler);
            }
            Ok(())
        })?;

        let result = trans.objectify_sequence(body);

        trans.env.drop_frame(n_macros);

        for (alias, old_name, bound) in rebound {
            alias.set_bound(bound);
            alias.set_name(old_name);
        }

        result
    }

    pub fn mul(args: &[Scm]) -> Result<Scm> {
        let mut total = Scm::Int(1);
        for &a in args {
            total = (total * a)?;
        }
        Ok(total)
    }

    pub fn add(args: &[Scm]) -> Result<Scm> {
        let mut total = Scm::Int(0);
        for &a in args {
            total = (total + a)?;
        }
        Ok(total)
    }

    pub fn sub(args: &[Scm]) -> Result<Scm> {
        match args {
            [] => Ok(Scm::Int(0)),
            [x] => (Scm::Int(0) - *x),
            [a, b @ ..] => {
                let mut result = *a;
                for &x in b {
                    result = (result - x)?;
                }
                Ok(result)
            }
        }
    }

    pub fn list(args: &[Scm]) -> Scm {
        Scm::list(args.iter().copied())
    }

    pub fn vector(args: &[Scm]) -> Scm {
        Scm::vector(args.iter().copied())
    }

    pub fn make_vector(k: usize, args: &[Scm]) -> Result<Scm> {
        let fill = match args {
            [] => Scm::uninitialized(),
            [f] => *f,
            _ => return Err(RuntimeError::IncorrectArity.into()),
        };
        Ok(Scm::vector(std::iter::repeat(fill).take(k.try_into()?)))
    }

    pub fn vector_fill(vec: Scm, fill: Scm, args: &[Scm]) -> Result<()> {
        let vec = vec.as_vector()?;
        let (start, end) = match args {
            [] => (0, vec.len()),
            [start] => (start.try_into()?, vec.len()),
            [start, end] => (start.try_into()?, end.try_into()?),
            _ => return Err(RuntimeError::IncorrectArity.into()),
        };
        for x in &vec[start..end] {
            x.set(fill);
        }
        Ok(())
    }

    pub fn disassemble(obj: Scm) {
        if let Scm::Closure(cls) = obj {
            println!("free variables: {:?}", cls.free_vars);
            println!("{:#?}", cls.code);
        }
    }

    pub fn list_globals(_args: &[Scm], context: &VirtualMachine) {
        for (i, value) in context.globals().iter().enumerate() {
            println!(
                "{:3} {} = {}",
                i,
                context.ga.find_var(i).full_name().display(),
                value.display()
            )
        }
    }

    pub fn timeit(args: &[Scm], context: &mut VirtualMachine) -> Result<Scm> {
        let start = Instant::now();
        context.invoke(args[0], &[])?;
        Ok(Scm::Int(start.elapsed().as_millis() as i64))
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use crate::library::LibraryBuilder;

        fn create_testing_context() -> Context {
            let mut ctx = Context::new();
            ctx.add_library("sunny/core", create_scheme_base_library());
            ctx.import_library("sunny/core");
            ctx.trans().mark_base_env();
            create_testing_libraries(ctx)
        }

        fn create_testing_libraries(mut ctx: Context) -> Context {
            ctx.add_library(
                "testing/1",
                LibraryBuilder::new()
                    .add_value("a", 1)
                    .add_value("b", 2)
                    .add_value(
                        "kons",
                        Scm::Primitive(RuntimePrimitive::new(
                            "cons",
                            Arity::Exact(2),
                            |args: &[Scm], _ctx| match &args[..] {
                                [a, b] => Ok(Scm::cons(a.into(), b.into())),
                                _ => unreachable!(),
                            },
                        )),
                    )
                    .add_value(
                        "kar",
                        Scm::Primitive(RuntimePrimitive::new(
                            "car",
                            Arity::Exact(1),
                            |args: &[Scm], _ctx| match &args[..] {
                                [_] => unimplemented!(),
                                _ => unreachable!(),
                            },
                        )),
                    )
                    .build(),
            );
            ctx.add_library(
                "testing/2",
                LibraryBuilder::new()
                    .add_macro(
                        "invoke",
                        MagicKeyword::new("invoke", |trans, expr| {
                            trans.objectify(expr.cdr().unwrap())
                        }),
                    )
                    .build(),
            );
            ctx
        }

        macro_rules! check {
            ($name:ident: $src:expr, $cmp:path) => {
                #[test]
                fn $name() {
                    let result = create_testing_context().eval_str($src).expect(concat!(
                        "Could not evaluate `",
                        $src,
                        "`"
                    ));
                    if !$cmp(&result) {
                        panic!(
                            r#"assertion failed: `(eq? actual expected)`
   expression: `{}`
 evaluates to: `{}`,
 but `{}` returned `false`"#,
                            $src,
                            result.display(),
                            stringify!($cmp)
                        )
                    }
                }
            };
        }

        macro_rules! compare {
            ($name:ident: $src:expr, $cmp:ident, $expect:expr) => {
                #[test]
                fn $name() {
                    let result = create_testing_context().eval_str($src).expect(concat!(
                        "Could not evaluate `",
                        $src,
                        "`"
                    ));
                    if !result.$cmp(&$expect) {
                        panic!(
                            r#"assertion failed: `(eq? actual expected)`
   expression: `{}`
 evaluates to: `{}`,
 but expected: `{}`"#,
                            $src,
                            result.display(),
                            $expect.display()
                        )
                    }
                }
            };
        }

        macro_rules! assert_error {
            ($name:ident: $src:expr, $expect:expr) => {
                #[test]
                fn $name() {
                    match create_testing_context().eval_str($src) {
                        Ok(r) => panic!(
                            r#"assertion failed:
         expression: `{}`
       evaluates to: `{}`,
 but expected error: `{:?}`"#,
                            $src,
                            r.display(),
                            $expect
                        ),
                        Err(e) if e == $expect => {}
                        Err(Error {
                            kind: crate::error::ErrorKind::Unhandled(e),
                            ..
                        }) if e == Scm::error($expect) => {}
                        Err(e) => panic!(
                            r#"assertion failed:
          expression: `{}`
        causes error: `{:?}`,
  but expected error: `{:?}`"#,
                            $src, e.kind, $expect
                        ),
                    }
                }
            };
        }

        mod self_evaluating {
            use super::*;

            compare!(boolean_true: "#t", equals, Scm::True);
            compare!(boolean_false: "#f", equals, Scm::False);
            compare!(nil: "'()", equals, Scm::Nil);
            compare!(integer: "42", equals, Scm::Int(42));
            compare!(negative_integer: "-42", equals, Scm::Int(-42));
            compare!(float: "3.1415", equals, Scm::Float(3.1415));
            compare!(symbol: "'foobar", ptr_eq, Scm::Symbol(Symbol::new("foobar")));
            compare!(string: "\"text\"", equals, Scm::str("text"));
            compare!(vector: "#(1 2 3)",
                     equals, Scm::vector(vec![Scm::Int(1), Scm::Int(2), Scm::Int(3)]));
            compare!(quoted_pair: "(quote (1 . 2))",
                     equals, Scm::cons(Scm::Int(1), Scm::Int(2)));
            compare!(quoted_list: "(quote (1 2 3))",
                     equals, Scm::list(vec![Scm::Int(1), Scm::Int(2), Scm::Int(3)]));
            compare!(quote_abbreviation: "(quote 'x)",
                     equals, Scm::list(vec![Scm::Symbol(Symbol::new("quote")),
                                            Scm::Symbol(Symbol::new("x"))]));
        }

        mod compound {
            use super::*;

            assert_error!(empty_sequence: "(begin)", ObjectifyErrorKind::ExpectedList);
            compare!(unary_sequence: "(begin 1)", equals, Scm::Int(1));
            compare!(binary_sequence: "(begin 1 2)", equals, Scm::Int(2));
            compare!(ternary_sequence: "(begin 1 2 3)", equals, Scm::Int(3));
            compare!(sequence_evaluates_sideeffects:
                     "((lambda (x) (begin (set! x 42) 2 x)) 0)", equals, Scm::Int(42));

            compare!(true_branch: "(if #t 1 2)", equals, Scm::Int(1));
            compare!(false_branch: "(if #f 1 2)", equals, Scm::Int(2));
            compare!(one_branch: "(if #t 1)", equals, Scm::Int(1));
            check!(no_else_branch_is_undefined: "(if #f 1)", Scm::is_undefined);
            compare!(if_does_not_evaluate_first_branch:
                     "((lambda (x) (if #f (set! x 99) 'f) x) 0)", equals, Scm::Int(0));
            compare!(if_does_not_evaluate_second_branch:
                     "((lambda (x) (if #t 't (set! x 99)) x) 0)", equals, Scm::Int(0));
        }

        mod abstraction {
            use super::*;

            compare!(fix_lambda: "((lambda () 123))", equals, Scm::Int(123));
            compare!(fix_lambda_with_args: "((lambda (x y) (cons x (cons y '()))) 7 5)",
                     equals, Scm::list(vec![Scm::Int(7), Scm::Int(5)]));
            compare!(fix_lambda_with_var_args: "((lambda (x y . z) (cons (cons x y) z)) 1 2 3 4)",
                     equals,
                     Scm::list(vec![Scm::cons(Scm::Int(1), Scm::Int(2)), Scm::Int(3), Scm::Int(4)]));
            compare!(lambda_as_arg: "((lambda (func) (func)) (lambda () 42))",
                     equals, Scm::Int(42));
            compare!(assign_lambda: "((lambda (func) (set! func (lambda () 753)) (func)) '*uninit*)",
                     equals, Scm::Int(753));
            compare!(assign_lambda_with_args:
                     "((lambda (func) (set! func (lambda (x y) (+ x y))) (func 5 2)) '*uninit*)",
                     equals, Scm::Int(7));
            compare!(assign_lambda_with_var_args:
                     "((lambda (func) (set! func (lambda abc abc)) (func 5 2)) '*uninit*)",
                     equals, Scm::list(vec![Scm::Int(5), Scm::Int(2)]));
            compare!(lambda_finds_free_variable_in_outer_scope:
                     "((lambda (outer) ((lambda () outer))) 42)", equals, Scm::Int(42));
            compare!(escaping_lambda_keeps_free_variable_from_outer_scope:
                     "(((lambda (outer) (lambda () outer)) 42))", equals, Scm::Int(42));
            compare!(lambda_can_modify_variable_in_outer_scope:
                     "((lambda (outer) ((lambda () (set! outer 12))) outer) 42)", equals, Scm::Int(12));
            compare!(escaping_lambda_can_modify_free_variable_from_outer_scope:
                     "((lambda (f init) (set! f (init 0)) (f) (f)) '*uninit* (lambda (n) (lambda () (set! n (+ n 1)) n)))",
                     equals, Scm::Int(2));
            compare!(shadowed_global_is_restored: "(begin (define foo 123) ((lambda (foo) foo) 42) foo)", equals, Scm::Int(123));
            compare!(shadowed_local_is_restored: "((lambda (x) ((lambda (x) x) 42) x) 123)", equals, Scm::Int(123));
        }

        mod variables {
            use super::*;
            use crate::error::RuntimeError;

            check!(reify_intrinsic: "cons", Scm::is_procedure);

            assert_error!(undefined_global_get: "flummox", RuntimeError::UndefinedGlobal(Scm::cons(Scm::symbol("/"), Scm::symbol("flummox"))));
            assert_error!(undefined_global_set: "(set! foo 42)", RuntimeError::UndefinedGlobal(Scm::cons(Scm::symbol("/"), Scm::symbol("foo"))));
            check!(new_global: "(define the-answer 42)", Scm::is_undefined);
            compare!(get_global: "(begin (define the-answer 42) the-answer)", equals, Scm::Int(42));
            compare!(overwrite_global: "(begin (define the-answer 42) (set! the-answer 666) the-answer)", equals, Scm::Int(666));

            compare!(return_local: "((lambda (x) x) 42)", equals, Scm::Int(42));
            compare!(local_shadows_predef: "((lambda (cons) cons) 42)", equals, Scm::Int(42));
            compare!(local_shadows_global: "(begin (define foo 123) ((lambda (foo) foo) 42))", equals, Scm::Int(42));
            compare!(local_shadows_local: "((lambda (foo) ((lambda (foo) foo) 123)) 42)", equals, Scm::Int(123));

            compare!(modify_local: "((lambda (x) (set! x 789) x) 42)", equals, Scm::Int(789));

            compare!(modify_free: "(((lambda (x) (lambda () (set! x 21) (+ x x))) 0))", equals, Scm::Int(42));
            compare!(access_free_while_setting: "(((lambda (x) (lambda () (set! x x) x)) 0))", equals, Scm::Int(0));

            compare!(immutable_redefinition: "(begin (define cons #f) cons)", equals, Scm::False);
            compare!(capture_free_variable_in_closure:
                r#"(begin
                    (define (outer x)
                        (lambda ()
                            (lambda () x)))
                    (((outer 42))))"#,
                 equals, Scm::Int(42));
        }

        mod application {
            use super::*;

            compare!(primitive: "(cons 1 2)", equals, Scm::cons(Scm::Int(1), Scm::Int(2)));

            compare!(fixed_nullary: "((lambda () 10))", equals, Scm::Int(10));
            compare!(fixed_unary: "((lambda (x) x) 20)", equals, Scm::Int(20));
            compare!(fixed_binary: "((lambda (x y) (cons x y)) 30 40)",
                     equals, Scm::cons(Scm::Int(30), Scm::Int(40)));
            compare!(fixed_ternary: "((lambda (x y z) (+ x (+ y z))) 10 20 30)",
                     equals, Scm::Int(60));
            compare!(fixed_nullary_vararg0: "((lambda x x))", equals, Scm::Nil);
            compare!(fixed_nullary_vararg1: "((lambda x x) 1)",
                     equals, Scm::list(vec![Scm::Int(1)]));
            compare!(fixed_nullary_vararg2: "((lambda x x) 1 2)",
                     equals, Scm::list(vec![Scm::Int(1), Scm::Int(2)]));
            compare!(fixed_unary_vararg0: "((lambda (y . x) (cons x y)) 1)",
                     equals, Scm::cons(Scm::Nil, Scm::Int(1)));
            compare!(fixed_unary_vararg1: "((lambda (y . x) (cons x y)) 1 2)",
                     equals, Scm::cons(Scm::list(vec![Scm::Int(2)]), Scm::Int(1)));
            compare!(fixed_binary_vararg0: "((lambda (z y . x) (cons x (+ y z))) 1 2)",
                     equals, Scm::cons(Scm::Nil, Scm::Int(3)));
            compare!(fixed_binary_vararg1: "((lambda (z y . x) (cons x (+ y z))) 1 2 3)",
                     equals, Scm::cons(Scm::list(vec![Scm::Int(3)]), Scm::Int(3)));

            compare!(lambda_nullary: "(begin (define func (lambda () 10)) (func))",
                     equals, Scm::Int(10));
            compare!(lambda_unary: "(begin (define func (lambda (x) x)) (func 20))",
                     equals, Scm::Int(20));
            compare!(lambda_binary: "(begin (define func (lambda (x y) (+ x y))) (func 5 25))",
                     equals, Scm::Int(30));
            compare!(lambda_nullary_vararg0: "(begin (define func (lambda x x)) (func))",
                     equals, Scm::Nil);
            compare!(lambda_nullary_vararg1: "(begin (define func (lambda x x)) (func 1))",
                     equals, Scm::list(vec![Scm::Int(1)]));
            compare!(lambda_unary_vararg1: "(begin (define func (lambda (x . y) (cons x y))) (func 1 2))",
                     equals, Scm::list(vec![Scm::Int(1), Scm::Int(2)]));
        }

        mod extra_syntax {
            use super::*;

            compare!(primitive: "(let ((x 42) (y 8)) (+ x y))", equals, Scm::Int(50));
        }

        mod macros {
            use super::*;
            use crate::error::RuntimeError;

            compare!(primitive:
                r#"(begin
                        (define-syntax force (syntax-rules () ((force x) (x))))
                        (force (lambda () 3)))"#,
                 equals, Scm::Int(3));

            assert_error!(hygiene_new_binding_undefined:
                r#"(begin
                        (define-syntax foo (syntax-rules () ((foo body) (let ((x 42)) body))))
                        (foo x))"#,
                RuntimeError::UndefinedGlobal(Scm::cons(Scm::symbol("/"), Scm::symbol("x"))));

            compare!(hygiene_new_binding_defined_with_let:
                r#"(begin
                        (define x 123)
                        (define-syntax foo (syntax-rules () ((foo body) (let ((x 42)) (cons x body)))))
                        (foo x))"#,
                 equals, Scm::cons(Scm::Int(42), Scm::Int(123)));

            compare!(hygiene_new_binding_defined_with_lambda:
                r#"(begin
                        (define x 123)
                        (define-syntax foo (syntax-rules () ((foo body) ((lambda (x) (cons x body)) 42))))
                        (foo x))"#,
                 equals, Scm::cons(Scm::Int(42), Scm::Int(123)));

            compare!(hygiene_preserve_definition_environment:
                r#"(begin
                        (define foo 42)
                        (define-syntax bar (syntax-rules () ((bar) foo)))
                        (let ((foo 0))
                             (bar)))"#,
                 equals, Scm::Int(42));

            compare!(bind_global_variable_not_value:
                r#"(begin
                        (define foo 123)
                        (define-syntax bar (syntax-rules () ((bar) foo)))
                        (define foo 42)
                        (bar))"#,
                 equals, Scm::Int(42));

            compare!(conflicting_identifiers:
                // this macro invocation should expand to (cons x x), where both x are different bindings.
                r#"(begin
                        (define x 1)
                        (define-syntax foo (syntax-rules () ((foo y) (cons x y))))
                        (let ((x 2))
                             (foo x)))"#,
                 equals, Scm::cons(Scm::Int(1), Scm::Int(2)));

            compare!(literals:
                r#"(begin
                        (define-syntax foo (syntax-rules (bar) ((foo bar) #t) ((foo _) #f)))
                        (cons (foo bar) (foo foo)))"#,
                 equals, Scm::cons(Scm::True, Scm::False));

            compare!(syntax_alternatives:
                r#"(begin
                        (define-syntax count (syntax-rules () ((_ a) 1) ((count a b) 2)))
                        (cons (count x) (count 7 8)))"#,
                 equals, Scm::cons(Scm::Int(1), Scm::Int(2)));

            compare!(rebind_identifier:
                r#"(begin
                        (define-syntax simple-let
                            (syntax-rules ()
                                ((_ name value form)
                                 ((lambda (name) form) value))))
                        (simple-let x 4 (+ x x)))"#,
                 equals, Scm::Int(8));

            compare!(ellipsis_simple:
                r#"(begin
                        (define-syntax add
                            (syntax-rules (/add)
                                ((_ /add) 0)
                                ((_ a /add) a)
                                ((_ a b ... /add) (+ a (add b ... /add)))))
                        (add 1 2 3 4 /add))"#,
                 equals, Scm::Int(10));

            compare!(custom_ellipsis:
                r#"(begin
                        (define-syntax add
                            (syntax-rules ** (/add)
                                ((_ /add) 0)
                                ((_ a /add) a)
                                ((_ a b ** /add) (+ a (add b **  /add)))))
                        (add 1 2 3 4 /add))"#,
                 equals, Scm::Int(10));

            compare!(match_empty_ellipsis:
                r#"(begin
                        (define-syntax add
                            (syntax-rules ()
                                ((_) 0)
                                ((_ a b ...) (+ a (add b ...)))))
                        (add 1 2 3 4))"#,
                 equals, Scm::Int(10));

            compare!(ellipsis_distribute_vars:
                r#"(begin
                        (define-syntax letme
                            (syntax-rules ()
                                ((_ ((name val) ...) body1 body2 ...)
                                 ((lambda (name ...) body1 body2 ...) val ...))))
                        (letme ((x 4) (y 6))
                               (+ x y)))"#,
                 equals, Scm::Int(10));

            compare!(let_syntax_allows_macro_in_body:
                r#"(define (force _) #f)
                   (let-syntax ((force (syntax-rules () ((force x) (x)))))
                     (force (lambda () 4)))"#,
                 equals, Scm::Int(4));

            compare!(let_syntax_limited_to_scope:
                r#"(define (force _) #f)
                   (let-syntax ((force (syntax-rules () ((force x) (x)))))
                     0)
                   (force (lambda () 4))"#,
                 equals, Scm::False);

            compare!(let_syntax_not_recursive:
                r#"(define (force _) #f)
                   (let-syntax ((force (syntax-rules () ((_ x) (x))))
                                (really-force (syntax-rules () ((_ x) (force x)))))
                     (really-force (lambda () 4)))"#,
                 equals, Scm::False);

            compare!(letrec_syntax_allow_recursive:
                r#"(define (force _) #f)
                   (letrec-syntax ((really-force (syntax-rules () ((_ x) (force x))))
                                   (force (syntax-rules () ((_ x) (x)))))
                     (really-force (lambda () 4)))"#,
                 equals, Scm::Int(4));

            compare!(nested_ellipses_empty_match:
                r#"(define-syntax nest (syntax-rules () ((_ (x ...) ...) '((x ...) ...))))
                   (nest)"#,
                 equals, Scm::Nil);

            compare!(nested_ellipses:
                r#"(define-syntax nest (syntax-rules () ((_ (x ...) ...) '((x ...) ...))))
                   (nest (1 2) (3 4 5))"#,
                 equals, Scm::list(vec![Scm::list(vec![Scm::Int(1), Scm::Int(2)]), Scm::list(vec![Scm::Int(3), Scm::Int(4), Scm::Int(5)])]));

            assert_error!(nested_ellipses_mismatch:
                r#"(define-syntax nest (syntax-rules () ((_ (x ...) ...) '((x ...) ))))
                   (nest (1 2) (3 4 5))"#,
                 ObjectifyErrorKind::MismatchedEllipses);

            compare!(macro_generating_macro:
                r#"(define-syntax be-like-begin
                      (syntax-rules ()
                        ((be-like-begin name)
                         (define-syntax name
                           (syntax-rules ()
                             ((name expr (... ...))
                              (begin expr (... ...))))))))

                   (be-like-begin sequence)
                   (sequence 1 2 3 4)"#,
                 equals, Scm::Int(4));

            compare!(submacro:
                r#"(define-syntax foo
                      (syntax-rules ()
                        ((foo)
                         (let-syntax ((tmp (syntax-rules ()
                                             ((tmp) 123))))
                            (tmp)))))
                   (foo)"#,
                 equals, Scm::Int(123));

            compare!(unquote_in_macro:
                r#"(import (sunny quasiquote))
                   (define-syntax foo
                      (syntax-rules ()
                        ((foo x)
                         `(x ,x))
                      ))
                   (let ((y 'zzz))
                     (foo y))"#,
                 equals, Scm::list(vec![Scm::symbol("y"), Scm::symbol("zzz")]));

            compare!(macro_expand_to_macro:
                r#"(define-syntax mylet
                      (syntax-rules ()
                        ((mylet x . body)
                         ((lambda (x) . body) 0))
                      ))
                   (define-syntax foo
                      (syntax-rules ()
                        ((foo)
                         (mylet z z))
                      ))
                   (foo)"#,
                 equals, Scm::Int(0));
        }

        mod libraries {
            use super::*;
            use crate::error::RuntimeError;

            assert_error!(nonexisting_library: "(import (test foo bar)) #f",
                ObjectifyErrorKind::UnknownLibrary(["test", "foo", "bar"].iter().collect()));

            compare!(import_and_do_nothing:
                r#"(import (testing 1)) #f"#,
                 equals, Scm::False);

            compare!(import_twice_and_do_nothing:
                r#"(import (testing 1)) (import (testing 1)) #f"#,
                 equals, Scm::False);

            compare!(import_values:
                r#"(import (testing 1)) (cons a b)"#,
                 equals, Scm::cons(Scm::Int(1), Scm::Int(2)));

            compare!(import_macros:
                r#"(import (testing 2)) (invoke (lambda () 0))"#,
                 equals, Scm::Int(0));

            assert_error!(import_only:
                r#"(import (only (testing 1) a)) a b"#,
                RuntimeError::UndefinedGlobal(Scm::cons(Scm::symbol("/"), Scm::symbol("b"))));

            assert_error!(import_except:
                r#"(import (except (testing 1) a)) a"#,
                RuntimeError::UndefinedGlobal(Scm::cons(Scm::symbol("/"), Scm::symbol("a"))));

            compare!(import_prefixed_values:
                r#"(import (prefix (testing 1) foo-)) (cons foo-a foo-b)"#,
                 equals, Scm::cons(Scm::Int(1), Scm::Int(2)));

            compare!(import_renamed_value:
                r#"(import (rename (testing 1) (b c))) (cons a c)"#,
                 equals, Scm::cons(Scm::Int(1), Scm::Int(2)));

            compare!(import_renamed_macro:
                r#"(import (rename (testing 2) (invoke call))) (call (lambda () 8))"#,
                 equals, Scm::Int(8));

            compare!(import_renamed_macro_from_lib:
                r#"(import (rename (testing macro) (mul mul_many))) (mul_many 2 3 4)"#,
                 equals, Scm::Int(24));

            compare!(import_nested_sets:
                r#"(import (rename (rename (testing 1) (a c)) (c x)))
                   x"#,
                 equals, Scm::Int(1));

            compare!(import_primitive:
                r#"(import (testing 1)) (kons 1 2)"#,
                 equals, Scm::cons(Scm::Int(1), Scm::Int(2)));

            compare!(import_user:
                r#"(import (testing lib)) (foo 42)"#,
                 equals, Scm::Int(42));

            compare!(import_user_macro:
                r#"(import (testing macro)) (force (delay 42))"#,
                 equals, Scm::Int(42));

            compare!(import_renamed_macro_export:
                r#"(import (testing macro)) (add_many 2 3 4)"#,
                 equals, Scm::Int(9));

            compare!(reexport:
                r#"(import (testing reexport)) (cons (foo 7) (kons 5 '()))"#,
                 equals, Scm::list(vec![Scm::Int(7), Scm::Int(5)]));
        }

        mod definition {
            use super::*;

            #[test]
            fn global_definition() {
                let mut ctx = create_testing_context();
                ctx.eval_str("(define x 42)").unwrap();
                assert_eq!(ctx.vm.globals().last(), Some(&(Scm::Int(42))));
            }

            #[test]
            fn local_definition() {
                let mut ctx = create_testing_context();
                assert_eq!(
                    ctx.eval_str("((lambda () (define x 42) x))").unwrap(),
                    Scm::Int(42)
                );
                assert_ne!(ctx.vm.globals().last().unwrap(), &(Scm::Int(42)));
            }

            #[test]
            fn define_syntax() {
                let mut ctx = create_testing_context();
                ctx.eval_str("(define (foo) 42)").unwrap();
                assert_eq!(ctx.eval_str("(foo)").unwrap(), Scm::Int(42));
            }

            #[test]
            fn define_syntax_local() {
                let mut ctx = create_testing_context();
                ctx.eval_str("(define (foo) (define (bar x) (* x 2)))")
                    .unwrap();
                assert_eq!(ctx.eval_str("((foo) 21)").unwrap(), Scm::Int(42));
            }
        }

        mod non_local_control_flow {
            use super::*;
            use crate::error::RuntimeError;

            compare!(let_cc:
                r#" (define cc #f)
                    (define bar 0)
                    (define result '())

                    (define (func)
                      (set! result (cons 0 result))
                      (let/cc k (set! cc k))
                      (set! bar (+ bar 1)))

                    (define (g)
                      (func)
                      (set! result (cons bar result)))

                    (g)
                    (if (< bar 3)
                        (cc))
                    result"#,
                equals, Scm::list(vec![Scm::Int(3), Scm::Int(2), Scm::Int(1), Scm::Int(0)]));

            compare!(let_ep:
                r#" (let/ep exit (exit 2) 3)"#,
                equals, Scm::Int(2));

            assert_error!(call_ep_invalid:
                r#" (define cnt #f)
                    (let/ep exit
                        (set! cnt exit))
                    (cnt 42)"#,
                RuntimeError::InvalidExitProcedure);

            compare!(dynamic_wind_trivial:
                r#"(import (sunny dynwind))
                   (define order '())
                   (cons (dynamic-wind (lambda () (set! order (cons 1 order)))
                                       (lambda () (set! order (cons 2 order)) 4)
                                       (lambda () (set! order (cons 3 order))))
                         order)"#,
                equals, Scm::list(vec![Scm::Int(4), Scm::Int(3), Scm::Int(2), Scm::Int(1)]));
        }

        mod bugs {
            use super::*;

            compare!(let_in_args:
                r#"(list (let ((x 1)) x)
                         (let ((y 2)) y))"#,
                equals, Scm::list(vec![Scm::Int(1), Scm::Int(2)]));

            compare!(raw_apply:
                r#"(apply (lambda (x y z) y) '(1 2 3))"#,
                equals, Scm::Int(2));

            compare!(assign_apply_result_to_variable:
                r#"(let ((x (apply (lambda (x y z) y) '(1 2 3)))) x)"#,
                equals, Scm::Int(2));
        }
    }
}
