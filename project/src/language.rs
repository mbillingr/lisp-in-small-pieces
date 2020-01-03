pub mod scheme {
    use crate::ast_transform::boxify::Boxify;
    use crate::ast_transform::flatten_closures::Flatten;
    use crate::ast_transform::generate_bytecode::BytecodeGenerator;
    use crate::ast_transform::transform_defines::TransformDefines;
    use crate::bytecode::{Closure, VirtualMachine};
    use crate::description::{Arity, FunctionDescription};
    use crate::env::Env;
    use crate::error::Result;
    use crate::objectify::{decons, Result as ObjectifyResult};
    use crate::objectify::{ocar, ocdr, Translate};
    use crate::objectify::{ObjectifyError, ObjectifyErrorKind};
    use crate::primitive::RuntimePrimitive;
    use crate::scm::{ResultWrap, Scm};
    use crate::sexpr::TrackedSexpr;
    use crate::source::Source;
    use crate::syntax::{Expression, MagicKeyword, PredefinedVariable};
    use std::ops::{Add, Div, Mul, Sub};

    pub struct Context {
        trans: Translate,
        vm: VirtualMachine,
    }

    impl Context {
        pub fn new() -> Self {
            let (env, runtime_predef) = init_env();

            let trans = Translate::new(env);

            let vm = VirtualMachine::new(vec![], runtime_predef);

            Context { trans, vm }
        }

        pub fn eval_str(&mut self, src: &str) -> Result<Scm> {
            self.eval_source(&src.into())
        }

        pub fn eval_source(&mut self, src: &Source) -> Result<Scm> {
            let sexpr = TrackedSexpr::from_source(src)?;
            self.eval_sexpr(&sexpr)
        }

        pub fn eval_sexpr(&mut self, sexpr: &TrackedSexpr) -> Result<Scm> {
            //println!("{:?}", self.trans);
            let ast = self.trans.objectify_toplevel(sexpr)?;
            let ast = ast.transform(&mut TransformDefines::new(self.trans.env.globals.clone()));
            let ast = ast.transform(&mut Boxify);
            //println!("{:#?}", ast);
            let ast = ast.transform(&mut Flatten::new());

            //println!("{:#?}", ast);

            let globals = self.trans.env.globals.clone();
            let predef = self.trans.env.predef.clone();

            let code = BytecodeGenerator::compile_toplevel(&ast, globals, predef);
            //println!("{:#?}", code);
            let code = Box::leak(Box::new(code));
            let closure = Box::leak(Box::new(Closure::simple(code)));

            self.vm.resize_globals(self.trans.env.globals.len());
            Ok(self.vm.eval(closure)?)
        }
    }

    macro_rules! predef {
        () => {
            (Env::new(), vec![])
        };

        (native $name:expr, =0, $func:expr; $($rest:tt)*) => {{
            predef!{
                primitive $name, =0,
                |args: &[Scm]| {
                    match &args[..] {
                        [] => $func(),
                        _ => unreachable!(),
                    }
                };
                $($rest)*}
        }};

        (native $name:expr, =1, $func:expr; $($rest:tt)*) => {{
            predef!{
                primitive $name, =1,
                |args: &[Scm]| {
                    match &args[..] {
                        [a] => $func(a.into()),
                        _ => unreachable!(),
                    }
                };
                $($rest)*}
        }};

        (native $name:expr, =2, $func:expr; $($rest:tt)*) => {{
            predef!{
                primitive $name, =2,
                |args: &[Scm]| {
                    match &args[..] {
                        [a, b] => $func(a.into(), b.into()),
                        _ => unreachable!(),
                    }
                };
                $($rest)*}
        }};

        (native $name:expr, =3, $func:expr; $($rest:tt)*) => {{
            predef!{
                primitive $name, =3,
                |args: &[Scm]| {
                    match &args[..] {
                        [a, b, c] => $func(a.into(), b.into(), c.into()),
                        _ => unreachable!(),
                    }
                };
                $($rest)*}
        }};

        (primitive $name:expr, =$arity:expr, $func:expr; $($rest:tt)*) => {{
            let (env, mut runtime_env) = predef!{$($rest)*};

            env.predef.extend(PredefinedVariable::new(
                                    $name,
                                    FunctionDescription::new(Arity::Exact($arity), $name))
                                .into());

            runtime_env.push(Scm::Primitive(RuntimePrimitive::new($name, Arity::Exact($arity),
                                                                  |args| $func(args).wrap())));

            (env, runtime_env)
        }};

        (primitive $name:expr, >=$arity:expr, $func:expr; $($rest:tt)*) => {{
            let (env, mut runtime_env) = predef!{$($rest)*};

            env.predef.extend(PredefinedVariable::new(
                                    $name,
                                    FunctionDescription::new(Arity::AtLeast($arity), $name))
                                .into());

            runtime_env.push(Scm::Primitive(RuntimePrimitive::new($name, Arity::AtLeast($arity),
                                                                  |args| $func(args).wrap())));

            (env, runtime_env)
        }};

        (macro $name:expr, $func:ident; $($rest:tt)*) => {{
            let (env, runtime_env) = predef!{$($rest)*};
            env.macros.extend(MagicKeyword::new($name, $func).into());
            (env, runtime_env)
        }};
    }

    pub fn init_env() -> (Env, Vec<Scm>) {
        predef! {
            native "cons", =2, Scm::cons;
            native "car", =1, Scm::car;
            native "cdr", =1, Scm::cdr;
            native "set-car!", =2, Scm::set_car;
            native "set-cdr!", =2, Scm::set_cdr;
            native "eq?", =2, Scm::ptr_eq;
            native "null?", =1, Scm::is_nil;
            native "<", =2, Scm::num_less;
            native "*", =2, Scm::mul;
            native "/", =2, Scm::div;
            native "+", =2, Scm::add;
            native "-", =2, Scm::sub;
            primitive "list", >=0, list;
            primitive "vector", >=0, vector;

            // non-standard stuff
            native "primitive?", =1, Scm::is_primitive;
            native "uninitialized?", =1, Scm::is_uninitialized;
            native "undefined?", =1, Scm::is_undefined;

            native "disassemble", =1, disassemble;

            macro "lambda", expand_lambda;
            macro "begin", expand_begin;
            macro "set!", expand_assign;
            macro "if", expand_alternative;
            macro "quote", expand_quote;
            macro "define", expand_define;
        }
    }

    pub fn expand_lambda(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> ObjectifyResult<Expression> {
        let def = &ocdr(expr)?;
        let names = ocar(def)?;
        let body = ocdr(def)?;
        trans.objectify_function(names, &body, env, expr.source().clone())
    }

    pub fn expand_begin(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> ObjectifyResult<Expression> {
        trans.objectify_sequence(&ocdr(expr)?, env)
    }

    pub fn expand_assign(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> ObjectifyResult<Expression> {
        let parts = expr.as_proper_list().ok_or(ObjectifyError {
            kind: ObjectifyErrorKind::ExpectedList,
            location: expr.source().clone(),
        })?;
        trans.objectify_assignment(&parts[1], &parts[2], env, expr.source().clone())
    }

    pub fn expand_quote(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> ObjectifyResult<Expression> {
        let body = &ocdr(expr)?;
        trans.objectify_quotation(ocar(body)?, env)
    }

    pub fn expand_alternative(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> ObjectifyResult<Expression> {
        let rest = ocdr(expr)?;
        let (cond, rest) = decons(&rest)?;
        let (yes, rest) = decons(&rest)?;
        let no = decons(&rest).ok().map(|(no, _)| no);
        trans.objectify_alternative(cond, yes, no, env, expr.source().clone())
    }

    pub fn expand_define(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> ObjectifyResult<Expression> {
        let def = ocdr(expr)?;
        let definee = ocar(&def)?;
        let form = ocdr(&def)?;

        if definee.is_symbol() {
            trans.objectify_definition(definee, ocar(&form)?, env, expr.source().clone())
        } else {
            unimplemented!()
        }
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

    #[cfg(test)]
    mod tests {
        use super::*;

        impl PartialEq for Scm {
            fn eq(&self, rhs: &Self) -> bool {
                self.equals(rhs)
            }
        }

        macro_rules! check {
            ($name:ident: $src:expr, $cmp:path) => {
                #[test]
                fn $name() {
                    let result = Context::new().eval_str($src).expect(concat!(
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
                    let result = Context::new().eval_str($src).expect(concat!(
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
                    match Context::new().eval_str($src) {
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

            check!(get_predefined: "cons", Scm::is_primitive);
            assert_error!(set_predefined: "(set! cons #f)", ObjectifyErrorKind::ImmutableAssignment);

            assert_error!(undefined_global_get: "flummox", RuntimeError::UndefinedGlobal);
            assert_error!(undefined_global_set: "(set! foo 42)", RuntimeError::UndefinedGlobal);
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

        mod definition {
            use super::*;

            #[test]
            fn global_definition() {
                let mut ctx = Context::new();
                ctx.eval_str("(define x 42)").unwrap();
                assert_eq!(ctx.vm.globals()[0], Scm::Int(42));
            }
        }
    }
}
