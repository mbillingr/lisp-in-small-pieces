pub mod scheme {
    use crate::ast::AstNode;
    use crate::ast::{MagicKeyword, RuntimePrimitive};
    use crate::ast_transform::boxify::Boxify;
    use crate::ast_transform::flatten_closures::Flatten;
    use crate::ast_transform::generate_bytecode::BytecodeGenerator;
    use crate::bytecode::{Closure, VirtualMachine};
    use crate::description::{Arity, FunctionDescription};
    use crate::env::Env;
    use crate::error::Error;
    use crate::objectify::{car, cdr, Translate};
    use crate::objectify::{decons, Result as ObjectifyResult};
    use crate::objectify::{ObjectifyError, ObjectifyErrorKind};
    use crate::scm::Scm;
    use crate::sexpr::TrackedSexpr;
    use crate::source::Source;
    use crate::Variable;

    type Result<T> = std::result::Result<T, Error>;

    pub struct Context {
        trans: Translate,
        vm: VirtualMachine,
    }

    impl Context {
        pub fn new() -> Self {
            let (env, runtime_predef) = init_env();

            let mut trans = Translate::new(env);

            let mut vm = VirtualMachine::new(vec![], runtime_predef);

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
            let ast = self.trans.objectify_toplevel(sexpr)?;
            let ast = ast.transform(&mut Boxify);
            //println!("{:#?}", ast);
            let ast = ast.transform(&mut Flatten::new());

            //println!("{:#?}", ast);

            let globals = self.trans.env.globals.clone();
            let predef = self.trans.env.predef.clone();

            let code = BytecodeGenerator::compile_toplevel(&ast, globals, predef);
            //println!("{:?}", code);
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

        (primitive $name:expr, =$arity:expr, $func:ident, $($rest:tt)*) => {{
            let (mut env, mut runtime_env) = predef!{$($rest)*};

            env.predef.extend(Variable::predefined(
                $name,
                FunctionDescription::new(Arity::Exact(2), $name),
            ));

            runtime_env.push(Scm::Primitive(RuntimePrimitive::new(Arity::Exact($arity), $func)));

            (env, runtime_env)
        }};

        (macro $name:expr, $func:ident, $($rest:tt)*) => {{
            let (mut env, mut runtime_env) = predef!{$($rest)*};
            env.predef.extend(Variable::Macro(MagicKeyword::new($name, $func)));
            runtime_env.push(Scm::Undefined);
            (env, runtime_env)
        }};
    }

    pub fn init_env() -> (Env, Vec<Scm>) {
        predef! {
            primitive "cons", =2, cons,
            primitive "eq?", =2, is_eq,
            primitive "<", =2, is_less,
            primitive "*", =2, multiply,
            primitive "/", =2, divide,
            primitive "+", =2, add,
            primitive "-", =2, subtract,

            macro "lambda", expand_lambda,
            macro "begin", expand_begin,
            macro "set!", expand_assign,
            macro "if", expand_alternative,
            macro "quote", expand_quote,
        }
    }

    pub fn expand_lambda(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> ObjectifyResult<AstNode> {
        let def = &cdr(expr)?;
        let names = car(def)?;
        let body = cdr(def)?;
        trans.objectify_function(names, &body, env, expr.source().clone())
    }

    pub fn expand_begin(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> ObjectifyResult<AstNode> {
        trans.objectify_sequence(&cdr(expr)?, env)
    }

    pub fn expand_assign(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> ObjectifyResult<AstNode> {
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
    ) -> ObjectifyResult<AstNode> {
        let body = &cdr(expr)?;
        trans.objectify_quotation(car(body)?, env)
    }

    pub fn expand_alternative(
        trans: &mut Translate,
        expr: &TrackedSexpr,
        env: &Env,
    ) -> ObjectifyResult<AstNode> {
        let rest = cdr(expr)?;
        let (cond, rest) = decons(&rest)?;
        let (yes, rest) = decons(&rest)?;
        let no = decons(&rest).ok().map(|(no, _)| no);
        trans.objectify_alternative(cond, yes, no, env, expr.source().clone())
    }

    pub fn cons(mut args: &[Scm]) -> Scm {
        let car = args[0];
        let cdr = args[1];
        Scm::cons(car, cdr)
    }

    pub fn is_eq(args: &[Scm]) -> Scm {
        match &args[..] {
            [a, b] => Scm::bool(Scm::ptr_eq(a, b)),
            _ => unreachable!(),
        }
    }

    pub fn is_less(args: &[Scm]) -> Scm {
        match &args[..] {
            [a, b] => Scm::bool(Scm::num_less(a, b).unwrap()),
            _ => unreachable!(),
        }
    }

    pub fn multiply(args: &[Scm]) -> Scm {
        match args[..] {
            [a, b] => (a * b).unwrap(),
            _ => unreachable!(),
        }
    }

    pub fn divide(args: &[Scm]) -> Scm {
        match args[..] {
            [a, b] => (a / b).unwrap(),
            _ => unreachable!(),
        }
    }

    pub fn add(args: &[Scm]) -> Scm {
        match args[..] {
            [a, b] => (a + b).unwrap_or_else(|_| panic!("attempt to add {:?} and {:?}", a, b)),
            _ => unreachable!(),
        }
    }

    pub fn subtract(args: &[Scm]) -> Scm {
        match args[..] {
            [a, b] => (a - b).unwrap(),
            _ => unreachable!(),
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
            use crate::symbol::Symbol;

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
            use crate::symbol::Symbol;

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
            compare!(shadowed_global_is_restored: "(begin (set! foo 123) ((lambda (foo) foo) 42) foo)", equals, Scm::Int(123));
            compare!(shadowed_local_is_restored: "((lambda (x) ((lambda (x) x) 42) x) 123)", equals, Scm::Int(123));
        }

        mod variables {
            use super::*;
            use crate::symbol::Symbol;

            check!(get_predefined: "cons", Scm::is_primitive);
            assert_error!(set_predefined: "(set! cons #f)", ObjectifyErrorKind::ImmutableAssignment);

            check!(undefined_global: "flummox", Scm::is_uninitialized);
            compare!(new_global: "(set! the-answer 42)", equals, Scm::Int(42));
            compare!(get_global: "(begin (set! the-answer 42) the-answer)", equals, Scm::Int(42));
            compare!(overwrite_global: "(begin (set! the-answer 42) (set! the-answer 666) the-answer)", equals, Scm::Int(666));

            compare!(return_local: "((lambda (x) x) 42)", equals, Scm::Int(42));
            compare!(local_shadows_predef: "((lambda (cons) cons) 42)", equals, Scm::Int(42));
            compare!(local_shadows_global: "(begin (set! foo 123) ((lambda (foo) foo) 42))", equals, Scm::Int(42));
            compare!(local_shadows_local: "((lambda (foo) ((lambda (foo) foo) 123)) 42)", equals, Scm::Int(123));

            compare!(modify_local: "((lambda (x) (set! x 789) x) 42)", equals, Scm::Int(789));

            compare!(modify_free: "(((lambda (x) (lambda () (set! x 21) (+ x x))) 0))", equals, Scm::Int(42));
            compare!(access_free_while_setting: "(((lambda (x) (lambda () (set! x x) x)) 0))", equals, Scm::Int(0));
        }

        mod application {
            use super::*;
            use crate::symbol::Symbol;

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

            compare!(lambda_nullary: "(begin (set! func (lambda () 10)) (func))",
                     equals, Scm::Int(10));
            compare!(lambda_unary: "(begin (set! func (lambda (x) x)) (func 20))",
                     equals, Scm::Int(20));
            compare!(lambda_binary: "(begin (set! func (lambda (x y) (+ x y))) (func 5 25))",
                     equals, Scm::Int(30));
            compare!(lambda_nullary_vararg0: "(begin (set! func (lambda x x)) (func))",
                     equals, Scm::Nil);
            compare!(lambda_nullary_vararg1: "(begin (set! func (lambda x x)) (func 1))",
                     equals, Scm::list(vec![Scm::Int(1)]));
            compare!(lambda_unary_vararg1: "(begin (set! func (lambda (x . y) (cons x y))) (func 1 2))",
                     equals, Scm::list(vec![Scm::Int(1), Scm::Int(2)]));
        }
    }
}
