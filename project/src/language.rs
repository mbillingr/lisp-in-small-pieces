pub mod scheme {
    use crate::ast_transform::boxify::Boxify;
    use crate::ast_transform::flatten_closures::Flatten;
    use crate::ast_transform::generate_bytecode::BytecodeGenerator;
    use crate::bytecode::{Closure, VirtualMachine};
    use crate::env::Env;
    use crate::error::Result;
    use crate::library::Library;
    use crate::macro_language::eval_syntax;
    use crate::objectify::{decons, Result as ObjectifyResult};
    use crate::objectify::{ocar, ocdr, Translate};
    use crate::objectify::{ObjectifyError, ObjectifyErrorKind};
    use crate::primitive::{Arity, RuntimePrimitive};
    use crate::scan_out_defines::{
        definition_value, definition_variable, make_function, scan_out_defines,
    };
    use crate::scm::{ResultWrap, Scm};
    use crate::sexpr::Sexpr::Symbol;
    use crate::sexpr::TrackedSexpr;
    use crate::source::Source;
    use crate::source::SourceLocation::NoSource;
    use crate::syntactic_closure::SyntacticClosure;
    use crate::syntax::{Expression, GlobalVariable, MagicKeyword, NoOp};
    use std::ops::{Add, Div, Mul, Sub};
    use std::path::PathBuf;
    use std::rc::Rc;

    pub struct Context {
        pub trans: Translate,
        pub vm: VirtualMachine,
    }

    impl Context {
        pub fn new() -> Self {
            let env = init_env();

            let trans = Translate::new(env);

            let vm = VirtualMachine::new(vec![]);

            Context { trans, vm }
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
            let ast = self.trans.objectify_toplevel(sexprs)?;
            let ast = ast.transform(&mut Boxify);
            let ast = ast.transform(&mut Flatten::new());

            //println!("{:#?}", ast);

            let code = BytecodeGenerator::compile_toplevel(&ast, &self.trans);
            //println!("{:#?}", code);
            let code = Box::leak(Box::new(code));
            let closure = Box::leak(Box::new(Closure::simple(code)));

            self.vm.synchronize_globals(self.trans.env.globals());
            Ok(self.vm.eval(closure)?)
        }

        fn add_library(&mut self, library_name: impl Into<PathBuf>, library: Library) {
            let lib = Rc::new(library);
            self.trans.add_library(library_name, lib.clone());
            self.vm.add_library(lib);
        }
    }

    macro_rules! predef {
        () => {
            Env::new()
        };

        (primitive $name:expr, =0, $func:expr; $($rest:tt)*) => {{
            predef!{
                @primitive $name, =0,
                |args: &[Scm], _ctx: &VirtualMachine| {
                    match &args[..] {
                        [] => $func(),
                        _ => unreachable!(),
                    }
                };
                $($rest)*}
        }};

        (primitive $name:expr, =1, $func:expr; $($rest:tt)*) => {{
            predef!{
                @primitive $name, =1,
                |args: &[Scm], _ctx: &VirtualMachine| {
                    match &args[..] {
                        [a] => $func(a.into()),
                        _ => unreachable!(),
                    }
                };
                $($rest)*}
        }};

        (primitive $name:expr, =2, $func:expr; $($rest:tt)*) => {{
            predef!{
                @primitive $name, =2,
                |args: &[Scm], _ctx: &VirtualMachine| {
                    match &args[..] {
                        [a, b] => $func(a.into(), b.into()),
                        _ => unreachable!(),
                    }
                };
                $($rest)*}
        }};

        (primitive $name:expr, =3, $func:expr; $($rest:tt)*) => {{
            predef!{
                @primitive $name, =3,
                |args: &[Scm], _ctx: &VirtualMachine| {
                    match &args[..] {
                        [a, b, c] => $func(a.into(), b.into(), c.into()),
                        _ => unreachable!(),
                    }
                };
                $($rest)*}
        }};

        (primitive $name:expr, >=$arity:expr, $func:expr; $($rest:tt)*) => {{
            let mut env = predef!{$($rest)*};
            let func = RuntimePrimitive::new($name, Arity::AtLeast($arity), |args, _ctx| $func(args).wrap());
            env.push_global(GlobalVariable::defined($name, Scm::primitive(func)));
            env
        }};

        (@primitive $name:expr, =$arity:expr, $func:expr; $($rest:tt)*) => {{
            let mut env = predef!{$($rest)*};
            let func = RuntimePrimitive::new($name, Arity::Exact($arity), |args, ctx| $func(args, ctx).wrap());
            env.push_global(GlobalVariable::defined($name, Scm::primitive(func)));
            env
        }};

        (intrinsic $name:expr, =1, $func:expr; $($rest:tt)*) => {{
            predef!{
                @intrinsic $name, =1,
                |args: &[Scm]| {
                    match &args[..] {
                        [a] => $func(a.into()),
                        _ => unreachable!(),
                    }
                };
                $($rest)*}
        }};

        (intrinsic $name:expr, =2, $func:expr; $($rest:tt)*) => {{
            predef!{
                @intrinsic $name, =2,
                |args: &[Scm], _ctx: &VirtualMachine| {
                    match &args[..] {
                        [a, b] => $func(a.into(), b.into()),
                        _ => unreachable!(),
                    }
                };
                $($rest)*}
        }};

        (@intrinsic $name:expr, =$arity:expr, $func:expr; $($rest:tt)*) => {{
            let mut env = predef!{$($rest)*};
            let func = RuntimePrimitive::new($name, Arity::Exact($arity), |args, ctx| $func(args, ctx).wrap());
            env.push_global(GlobalVariable::defined($name, Scm::intrinsic(func)));
            env
        }};

        (macro $name:expr, $func:ident; $($rest:tt)*) => {{
            let mut env = predef!{$($rest)*};
            env.push_global(MagicKeyword::new($name, $func));
            env
        }};
    }

    pub fn init_env() -> Env {
        predef! {
            intrinsic "cons", =2, Scm::cons;
            primitive "car", =1, Scm::car;
            primitive "cdr", =1, Scm::cdr;
            primitive "set-car!", =2, Scm::set_car;
            primitive "set-cdr!", =2, Scm::set_cdr;
            primitive "eq?", =2, Scm::ptr_eq;
            primitive "null?", =1, Scm::is_nil;
            primitive "<", =2, Scm::num_less;
            primitive "*", =2, Scm::mul;
            primitive "/", =2, Scm::div;
            primitive "+", =2, Scm::add;
            primitive "-", =2, Scm::sub;
            primitive "list", >=0, list;
            primitive "vector", >=0, vector;

            // non-standard stuff
            primitive "primitive?", =1, Scm::is_primitive;
            primitive "uninitialized?", =1, Scm::is_uninitialized;
            primitive "undefined?", =1, Scm::is_undefined;

            primitive "disassemble", =1, disassemble;
            @primitive "globals", =0, list_globals;

            macro "lambda", expand_lambda;
            macro "let", expand_let;
            macro "begin", expand_begin;
            macro "set!", expand_assign;
            macro "if", expand_alternative;
            macro "quote", expand_quote;
            macro "define", expand_define;
            macro "define-syntax", expand_define_syntax;
            macro "or", expand_or;
        }
    }

    pub fn expand_or(trans: &mut Translate, expr: &TrackedSexpr) -> ObjectifyResult<Expression> {
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
    }

    pub fn expand_lambda(
        trans: &mut Translate,
        expr: &TrackedSexpr,
    ) -> ObjectifyResult<Expression> {
        let def = &ocdr(expr)?;
        let names = ocar(def)?;
        let body = ocdr(def)?;
        let body = scan_out_defines(body.clone())?;
        trans.objectify_function(names, &body, expr.source().clone())
    }

    pub fn expand_let(trans: &mut Translate, expr: &TrackedSexpr) -> ObjectifyResult<Expression> {
        let def = ocdr(expr)?;
        let vars = ocar(def)?;
        let body = ocdr(def)?;

        let mut var_names = vec![];
        let mut var_forms = vec![];
        vars.scan(|v| {
            v.at(0)
                .and_then(|name| v.at(1).map(|form| (name, form)))
                .ok_or_else(|| ObjectifyError {
                    kind: ObjectifyErrorKind::SyntaxError,
                    location: body.source().clone(),
                })
                .map(|(name, form)| {
                    var_names.push(name.clone());
                    var_forms.push(form.clone());
                })
        })?;

        let param_list = TrackedSexpr::list(var_names, vars.src.clone());
        let func = make_function(param_list, body.clone(), expr.src.clone());

        let mut call = vec![func];
        call.extend(var_forms);

        let new_expr = TrackedSexpr::list(call, expr.src.clone());
        trans.objectify(&new_expr)
    }

    pub fn expand_begin(trans: &mut Translate, expr: &TrackedSexpr) -> ObjectifyResult<Expression> {
        trans.objectify_sequence(ocdr(expr)?)
    }

    pub fn expand_assign(
        trans: &mut Translate,
        expr: &TrackedSexpr,
    ) -> ObjectifyResult<Expression> {
        expr.at(1)
            .and_then(|variable| expr.at(2).map(|expr| (variable, expr)))
            .ok_or(ObjectifyError {
                kind: ObjectifyErrorKind::ExpectedList,
                location: expr.source().clone(),
            })
            .and_then(|(variable, expr)| {
                trans.objectify_assignment(variable, expr, expr.source().clone())
            })
    }

    pub fn expand_quote(trans: &mut Translate, expr: &TrackedSexpr) -> ObjectifyResult<Expression> {
        let body = &ocdr(expr)?;
        trans.objectify_quotation(ocar(body)?)
    }

    pub fn expand_alternative(
        trans: &mut Translate,
        expr: &TrackedSexpr,
    ) -> ObjectifyResult<Expression> {
        let rest = ocdr(expr)?;
        let (cond, rest) = decons(&rest)?;
        let (yes, rest) = decons(&rest)?;
        let no = decons(&rest).ok().map(|(no, _)| no);
        trans.objectify_alternative(cond, yes, no, expr.source().clone())
    }

    pub fn expand_define(
        trans: &mut Translate,
        expr: &TrackedSexpr,
    ) -> ObjectifyResult<Expression> {
        let definee = definition_variable(expr)?;
        let form = definition_value(expr)?;
        trans.objectify_definition(definee, &form, expr.source().clone())
    }

    pub fn expand_define_syntax(
        trans: &mut Translate,
        expr: &TrackedSexpr,
    ) -> ObjectifyResult<Expression> {
        let (name, syntax) = expr
            .at(1)
            .and_then(|name| name.as_symbol().copied())
            .and_then(|name| expr.at(2).map(|syntax| (name, syntax)))
            .ok_or_else(|| ObjectifyError {
                kind: ObjectifyErrorKind::SyntaxError,
                location: expr.source().clone(),
            })?;
        let handler = eval_syntax(syntax, &trans.env)?;
        let macro_binding = MagicKeyword { name, handler };
        trans.env.push_local(macro_binding);
        Ok(Expression::NoOp(NoOp))
    }

    pub fn list(args: &[Scm]) -> Scm {
        Scm::list(args.iter().copied())
    }

    pub fn vector(args: &[Scm]) -> Scm {
        Scm::vector(args.iter().copied())
    }

    pub fn disassemble(obj: Scm) {
        if let Scm::Closure(cls) = obj {
            println!("free variables: {:?}", cls.free_vars);
            println!("{:#?}", cls.code);
        }
    }

    pub fn list_globals(args: &[Scm], context: &VirtualMachine) {
        for (i, (value, name)) in context.globals().iter().enumerate() {
            println!("{:3} {} = {}", i, name, value)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use crate::library::LibraryBuilder;

        impl PartialEq for Scm {
            fn eq(&self, rhs: &Self) -> bool {
                self.equals(rhs)
            }
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
                            "kons",
                            Arity::Exact(2),
                            |args: &[Scm], _ctx| match &args[..] {
                                [a, b] => Ok(Scm::cons(a.into(), b.into())),
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
                    let result = create_testing_libraries(Context::new())
                        .eval_str($src)
                        .expect(concat!("Could not evaluate `", $src, "`"));
                    if !$cmp(&result) {
                        panic!(
                            r#"assertion failed: `(eq? actual expected)`
   expression: `{}`
 evaluates to: `{}`,
 but `{}` returned `false`"#,
                            $src,
                            result,
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
                    let result = create_testing_libraries(Context::new())
                        .eval_str($src)
                        .expect(concat!("Could not evaluate `", $src, "`"));
                    if !result.$cmp(&$expect) {
                        panic!(
                            r#"assertion failed: `(eq? actual expected)`
   expression: `{}`
 evaluates to: `{}`,
 but expected: `{}`"#,
                            $src, result, $expect
                        )
                    }
                }
            };
        }

        macro_rules! assert_error {
            ($name:ident: $src:expr, $expect:expr) => {
                #[test]
                fn $name() {
                    match create_testing_libraries(Context::new()).eval_str($src) {
                        Ok(r) => panic!(
                            r#"assertion failed:
         expression: `{}`
       evaluates to: `{}`,
 but expected error: `{:?}`"#,
                            $src, r, $expect
                        ),
                        Err(e) if e == $expect => {}
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
            use crate::symbol::Symbol;

            compare!(boolean_true: "#t", equals, Scm::True);
            compare!(boolean_false: "#f", equals, Scm::False);
            compare!(nil: "'()", equals, Scm::Nil);
            compare!(integer: "42", equals, Scm::Int(42));
            compare!(float: "3.1415", equals, Scm::Float(3.1415));
            compare!(symbol: "'foobar", ptr_eq, Scm::Symbol(Symbol::new("foobar")));
            compare!(string: "\"text\"", equals, Scm::String("text"));
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
            use crate::symbol::Symbol;

            check!(reify_intrinsic: "cons", Scm::is_procedure);

            assert_error!(undefined_global_get: "flummox", RuntimeError::UndefinedGlobal(Symbol::new("flummox")));
            assert_error!(undefined_global_set: "(set! foo 42)", RuntimeError::UndefinedGlobal(Symbol::new("foo")));
            compare!(new_global: "(define the-answer 42)", equals, Scm::Int(42));
            compare!(get_global: "(begin (define the-answer 42) the-answer)", equals, Scm::Int(42));
            compare!(overwrite_global: "(begin (define the-answer 42) (set! the-answer 666) the-answer)", equals, Scm::Int(666));

            compare!(return_local: "((lambda (x) x) 42)", equals, Scm::Int(42));
            compare!(local_shadows_predef: "((lambda (cons) cons) 42)", equals, Scm::Int(42));
            compare!(local_shadows_global: "(begin (define foo 123) ((lambda (foo) foo) 42))", equals, Scm::Int(42));
            compare!(local_shadows_local: "((lambda (foo) ((lambda (foo) foo) 123)) 42)", equals, Scm::Int(123));

            compare!(modify_local: "((lambda (x) (set! x 789) x) 42)", equals, Scm::Int(789));

            compare!(modify_free: "(((lambda (x) (lambda () (set! x 21) (+ x x))) 0))", equals, Scm::Int(42));
            compare!(access_free_while_setting: "(((lambda (x) (lambda () (set! x x) x)) 0))", equals, Scm::Int(0));
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

            compare!(primitive: "(let ((x 1) (y 2)) (+ x y))", equals, Scm::Int(3));
        }

        mod syntactic_closure {
            use super::*;
            compare!(or:
                r#"(begin
                        (define temp 42)
                        (or #f temp))"#,
                equals, Scm::Int(42));
        }

        mod macros {
            use super::*;
            use crate::error::RuntimeError;
            use crate::symbol::Symbol;

            compare!(primitive:
                r#"(begin
                        (define-syntax force (syntax-rules () ((force x) (x))))
                        (force (lambda () 3)))"#,
                 equals, Scm::Int(3));

            assert_error!(hygiene_new_binding_undefined:
                r#"(begin
                        (define-syntax foo (syntax-rules () ((foo body) (let ((x 42)) body))))
                        (foo x))"#,
                RuntimeError::UndefinedGlobal(Symbol::new("x")));

            compare!(hygiene_new_binding_defined:
                r#"(begin
                        (define x 123)
                        (define-syntax foo (syntax-rules () ((foo body) (let ((x 42)) body))))
                        (foo x))"#,
                 equals, Scm::Int(123));

            compare!(hygiene_preserve_definition_environment:
                r#"(begin
                        (define foo 42)
                        (define-syntax bar (syntax-rules () ((bar) foo)))
                        (let ((foo 0))
                             (bar)))"#,
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
        }

        mod libraries {
            use super::*;
            use crate::error::RuntimeError;
            use crate::symbol::Symbol;

            assert_error!(nonexisting_library: "(import (foo bar)) #f",
                ObjectifyErrorKind::UnknownLibrary(["foo", "bar"].iter().collect()));

            compare!(import_and_do_nothing:
                r#"(import (testing 1)) #f"#,
                 equals, Scm::False);

            compare!(import_values:
                r#"(import (testing 1)) (cons a b)"#,
                 equals, Scm::cons(Scm::Int(1), Scm::Int(2)));

            compare!(import_macros:
                r#"(import (testing 2)) (invoke (lambda () 0))"#,
                 equals, Scm::Int(0));

            assert_error!(import_only:
                r#"(import (only (testing 1) a)) a b"#,
                RuntimeError::UndefinedGlobal(Symbol::new("b")));

            assert_error!(import_except:
                r#"(import (except (testing 1) a)) a"#,
                RuntimeError::UndefinedGlobal(Symbol::new("a")));

            compare!(import_prefixed_values:
                r#"(import (prefix (testing 1) foo-)) (cons foo-a foo-b)"#,
                 equals, Scm::cons(Scm::Int(1), Scm::Int(2)));

            compare!(import_renamed_value:
                r#"(import (rename (testing 1) (b c))) (cons a c)"#,
                 equals, Scm::cons(Scm::Int(1), Scm::Int(2)));

            compare!(import_shared_value:
                r#"(import (testing 1))
                   (import (rename (testing 1) (a c)))
                   (set! c 42)
                   (cons a c)"#,
                 equals, Scm::cons(Scm::Int(42), Scm::Int(42)));

            compare!(import_nested_sets:
                r#"(import (rename (rename (testing 1) (a c)) (c x)))
                   x"#,
                 equals, Scm::Int(1));

            compare!(import_primitive:
                r#"(import (testing 1)) (kons 1 2)"#,
                 equals, Scm::cons(Scm::Int(0), Scm::Int(2))); // todo: possible to import primitives into predef?
        }

        mod definition {
            use super::*;
            use crate::symbol::Symbol;

            #[test]
            fn global_definition() {
                let mut ctx = Context::new();
                ctx.eval_str("(define x 42)").unwrap();
                assert_eq!(
                    ctx.vm.globals().last(),
                    Some(&(Scm::Int(42), Symbol::new("x")))
                );
            }

            #[test]
            fn local_definition() {
                let mut ctx = Context::new();
                assert_eq!(
                    ctx.eval_str("((lambda () (define x 42) x))").unwrap(),
                    Scm::Int(42)
                );
                assert_ne!(
                    ctx.vm.globals().last().unwrap(),
                    &(Scm::Int(42), Symbol::new("x"))
                );
            }

            #[test]
            fn define_syntax() {
                let mut ctx = Context::new();
                ctx.eval_str("(define (foo) 42)").unwrap();
                assert_eq!(ctx.eval_str("(foo)").unwrap(), Scm::Int(42));
            }

            #[test]
            fn define_syntax_local() {
                let mut ctx = Context::new();
                ctx.eval_str("(define (foo) (define (bar x) (* x 2)))")
                    .unwrap();
                assert_eq!(ctx.eval_str("((foo) 21)").unwrap(), Scm::Int(42));
            }
        }
    }
}
