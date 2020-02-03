use crate::env::Env;
use crate::error::{Error, Result};
use crate::objectify::{ObjectifyErrorKind, Translate};
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::source::SourceLocation::NoSource;
use crate::symbol::Symbol;
use crate::syntactic_closure::SyntacticClosure;
use crate::syntax::{Expression, MagicKeywordHandler};
use std::collections::HashMap;
use std::rc::Rc;

pub fn eval_syntax(expr: &TrackedSexpr, env: &Env) -> Result<MagicKeywordHandler> {
    match expr.at(0) {
        Ok(s) if s == "syntax-rules" => {
            if let Ok(ellipsis) = expr.at(1).unwrap().as_symbol() {
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
    definition_env: Env,
) -> Result<MagicKeywordHandler> {
    Ok(Rc::new(
        move |trans: &mut Translate, expr: &TrackedSexpr| -> Result<Expression> {
            let sexpr = apply_syntax_rules(
                expr,
                ellipsis,
                &literals,
                &rules,
                &trans.env,
                &definition_env,
            )?;

            trans.objectify(&sexpr)
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

        if let Some(bound_vars) = match_pattern(
            expr.cdr().unwrap(),
            ellipsis,
            literals,
            pattern.cdr().unwrap(),
        ) {
            let bound_vars = bound_vars
                .into_iter()
                .map(|(name, x)| (name, SyntacticClosure::new(x, env.clone()).into()))
                .collect();
            let result = realize_template(template.clone(), &bound_vars, env, definition_env)?;
            Ok(SyntacticClosure::new(result, definition_env.clone()).into())
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
        Err(Error::at_expr(ObjectifyErrorKind::SyntaxError, expr))
    }
}

fn match_pattern(
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
                let mut a = match_pattern(&x.0, ellipsis, literals, &p.0)?;
                let b = match_pattern(&x.1, ellipsis, literals, &p.1)?;
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
    template: TrackedSexpr,
    bound_vars: &HashMap<Symbol, TrackedSexpr>,
    env: &Env,
    definition_env: &Env,
) -> Result<TrackedSexpr> {
    use Sexpr::*;
    match &template.sexpr {
        Symbol(s) => Ok(bound_vars.get(&s).cloned().unwrap_or(template)),
        Pair(_) => {
            let (car, cdr) = template.decons().unwrap();
            Ok(TrackedSexpr::cons(
                realize_template(car, bound_vars, env, definition_env)?,
                realize_template(cdr, bound_vars, env, definition_env)?,
                NoSource,
            ))
        }
        _ => Ok(template),
    }
}
