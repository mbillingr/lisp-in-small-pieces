use crate::env::Env;
use crate::objectify::{ObjectifyError, ObjectifyErrorKind, Result, Translate};
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::source::SourceLocation::NoSource;
use crate::symbol::Symbol;
use crate::syntax::{
    Expression, GlobalReference, LocalReference, MagicKeywordHandler, PredefinedReference,
};
use crate::utils::Named;
use std::collections::HashMap;
use std::rc::Rc;

pub static CAPTURED_BINDING_MARKER: Symbol = Symbol::uninterned("bound-syntax");

pub fn eval_syntax(expr: &TrackedSexpr, env: &Env) -> Result<MagicKeywordHandler> {
    match expr.at(0) {
        Some(s) if s == "syntax-rules" => {
            if let Some(ellipsis) = expr.at(1).unwrap().as_symbol() {
                let literals = expr.at(2).unwrap();
                let rules = expr.cdr().unwrap().cdr().unwrap().cdr().unwrap();
                eval_syntax_rules(*ellipsis, literals.clone(), rules.clone(), env.deep_clone())
            } else {
                let literals = expr.at(1).unwrap();
                let rules = expr.cdr().unwrap().cdr().unwrap();
                eval_syntax_rules(
                    Symbol::new("..."),
                    literals.clone(),
                    rules.clone(),
                    env.deep_clone(),
                )
            }
        }
        _ => panic!("Syntax error in macro definition: {}", expr),
    }
}

pub fn eval_syntax_rules(
    ellipsis: Symbol,
    literals: TrackedSexpr,
    rules: TrackedSexpr,
    definition_env: Env,
) -> Result<MagicKeywordHandler> {
    Ok(Rc::new(
        move |trans: &mut Translate, expr: &TrackedSexpr, env: &Env| -> Result<Expression> {
            let sexpr =
                apply_syntax_rules(expr, ellipsis, &literals, &rules, env, &definition_env)?;
            //println!("{} -> {}", expr, sexpr);

            let n = env.syntax.len();

            for i in 0..definition_env.macros.len() {
                env.syntax.extend(definition_env.macros.at(i).into())
            }

            for i in 0..definition_env.predef.len() {
                env.syntax.extend(definition_env.predef.at(i).into())
            }

            for i in 0..definition_env.globals.len() {
                env.syntax.extend(definition_env.globals.at(i).into())
            }

            for i in 0..definition_env.locals.len() {
                env.syntax.extend(definition_env.locals.at(i).into())
            }

            let result = trans.objectify(&sexpr, env);

            env.syntax.pop_frame(env.syntax.len() - n);

            result
        },
    ))
}

fn apply_syntax_rules(
    expr: &TrackedSexpr,
    ellipsis: Symbol,
    literals: &TrackedSexpr,
    rules: &TrackedSexpr,
    env: &Env,
    definition_env: &Env,
) -> Result<TrackedSexpr> {
    if rules.is_pair() {
        let rule = rules.car().unwrap();
        let pattern = rule.at(0).unwrap();
        let template = rule.at(1).unwrap();

        if let Some(bound_vars) = match_rule(
            expr.cdr().unwrap(),
            ellipsis,
            literals,
            pattern.cdr().unwrap(),
        ) {
            realize_template(template, &bound_vars, definition_env)
        } else {
            apply_syntax_rules(
                expr,
                ellipsis,
                literals,
                rules.cdr().unwrap(),
                env,
                definition_env,
            )
        }
    } else {
        Err(ObjectifyError {
            kind: ObjectifyErrorKind::SyntaxError,
            location: expr.src.clone(),
        })
    }
}

fn match_rule(
    expr: &TrackedSexpr,
    ellipsis: Symbol,
    literals: &TrackedSexpr,
    rule: &TrackedSexpr,
) -> Option<HashMap<Symbol, TrackedSexpr>> {
    //println!("matching {} and {}", expr, rule);
    use Sexpr::*;
    match &rule.sexpr {
        lit if literals.contains(lit) => {
            if lit == &expr.sexpr {
                Some(HashMap::new())
            } else {
                None
            }
        }
        Symbol(s) if *s == ellipsis => unimplemented!(),
        Symbol(s) => {
            let mut binding = HashMap::new();
            binding.insert(*s, expr.clone());
            Some(binding)
        }
        Pair(p) => {
            if let Pair(x) = &expr.sexpr {
                let mut a = match_rule(&x.0, ellipsis, literals, &p.0)?;
                let b = match_rule(&x.1, ellipsis, literals, &p.1)?;
                a.extend(b);
                Some(a)
            } else {
                None
            }
        }
        Vector(_) => unimplemented!(),
        _ if rule == expr => Some(HashMap::new()),
        _ => None,
    }
}

fn realize_template(
    template: &TrackedSexpr,
    bound_vars: &HashMap<Symbol, TrackedSexpr>,
    definition_env: &Env,
) -> Result<TrackedSexpr> {
    use Sexpr::*;
    match &template.sexpr {
        Symbol(s) => Ok(bound_vars
            .get(&s)
            .cloned()
            .or_else(|| {
                definition_env
                    .find_variable(s)
                    .map(|var| mark_captured_binding(template.clone()))
                    .or_else(|| {
                        Some(TrackedSexpr {
                            sexpr: Symbol(s.as_uninterned()),
                            src: NoSource,
                        })
                    })
            })
            .unwrap()
            .with_src(NoSource)),
        Pair(p) => Ok(TrackedSexpr::cons(
            realize_template(&p.0, bound_vars, definition_env)?,
            realize_template(&p.1, bound_vars, definition_env)?,
            NoSource,
        )),
        _ => Ok(template.clone()),
    }
}

fn mark_captured_binding(keyword: TrackedSexpr) -> TrackedSexpr {
    TrackedSexpr::cons(
        TrackedSexpr {
            sexpr: Sexpr::Symbol(CAPTURED_BINDING_MARKER),
            src: NoSource,
        },
        keyword,
        NoSource,
    )
}

pub fn is_captured_binding(expr: &TrackedSexpr) -> bool {
    expr.car()
        .and_then(TrackedSexpr::as_symbol)
        .map(|&s| s == CAPTURED_BINDING_MARKER)
        .unwrap_or(false)
}

pub fn expand_captured_binding(
    trans: &mut Translate,
    expr: &TrackedSexpr,
    env: &Env,
) -> Result<Expression> {
    use crate::syntax::Variable::*;
    let name = expr.cdr().and_then(TrackedSexpr::as_symbol).unwrap();
    match env.syntax.find_variable(name) {
        Some(LocalVariable(v)) => Ok(LocalReference::new(v, expr.source().clone()).into()),
        Some(GlobalVariable(v)) => Ok(GlobalReference::new(v, expr.source().clone()).into()),
        Some(PredefinedVariable(v)) => {
            Ok(PredefinedReference::new(v, expr.source().clone()).into())
        }
        Some(MagicKeyword(mkw)) => Ok((mkw).into()),
        Some(FreeVariable(_)) => unreachable!(),
        None => unimplemented!("{}", expr),
    }
}
