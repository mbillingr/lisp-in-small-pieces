use crate::env::Env;
use crate::objectify::{ObjectifyError, ObjectifyErrorKind, Result, Translate};
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::source::SourceLocation::NoSource;
use crate::symbol::Symbol;
use crate::syntax::{Expression, MagicKeywordHandler};
use std::collections::HashMap;
use std::rc::Rc;

pub fn eval_syntax(expr: &TrackedSexpr, env: &Env) -> Result<MagicKeywordHandler> {
    match expr.at(0) {
        Some(s) if s == "syntax-rules" => {
            if let Some(ellipsis) = expr.at(1).unwrap().as_symbol() {
                let literals = expr.at(2).unwrap();
                let rules = expr.cdr().unwrap().cdr().unwrap().cdr().unwrap();
                eval_syntax_rules(*ellipsis, literals.clone(), rules.clone(), env.clone())
            } else {
                let literals = expr.at(1).unwrap();
                let rules = expr.cdr().unwrap().cdr().unwrap();
                eval_syntax_rules(
                    Symbol::new("..."),
                    literals.clone(),
                    rules.clone(),
                    env.clone(),
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
    _definition_env: Env,
) -> Result<MagicKeywordHandler> {
    Ok(Rc::new(
        move |trans: &mut Translate, expr: &TrackedSexpr, env: &Env| -> Result<Expression> {
            let sexpr = apply_syntax_rules(expr, ellipsis, &literals, &rules, env)?;
            trans.objectify(&sexpr, env)
        },
    ))
}

fn apply_syntax_rules(
    expr: &TrackedSexpr,
    ellipsis: Symbol,
    literals: &TrackedSexpr,
    rules: &TrackedSexpr,
    env: &Env,
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
            realize_template(template, &bound_vars)
        } else {
            apply_syntax_rules(expr, ellipsis, literals, rules.cdr().unwrap(), env)
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
) -> Result<TrackedSexpr> {
    use Sexpr::*;
    match &template.sexpr {
        Symbol(s) => Ok(bound_vars.get(&s).expect("unbound macro variable").clone()),
        Pair(p) => Ok(TrackedSexpr::cons(
            realize_template(&p.0, bound_vars)?,
            realize_template(&p.1, bound_vars)?,
            NoSource,
        )),
        _ => Ok(template.clone()),
    }
}
