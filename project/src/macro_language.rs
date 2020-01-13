use crate::env::Env;
use crate::objectify::{ObjectifyError, ObjectifyErrorKind, Result, Translate};
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::source::SourceLocation::NoSource;
use crate::symbol::Symbol;
use crate::syntax::{
    Expression, GlobalReference, LocalReference, MagicKeywordHandler, PredefinedReference, Variable,
};
use std::collections::{HashMap, HashSet};
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
    let mut captures = Vec::new();
    let rules = prepare_syntax_rules(ellipsis, &literals, rules, &definition_env, &mut captures)?;

    Ok(Rc::new(
        move |trans: &mut Translate, expr: &TrackedSexpr, env: &Env| -> Result<Expression> {
            let sexpr =
                apply_syntax_rules(expr, ellipsis, &literals, &rules, env, &definition_env)?;
            //println!("{} -> {}", expr, sexpr);

            env.syntax.extend_frame(captures.clone().into_iter());

            let result = trans.objectify(&sexpr, env);

            env.syntax.pop_frame(captures.len());

            result
        },
    ))
}

fn prepare_syntax_rules(
    ellipsis: Symbol,
    literals: &TrackedSexpr,
    rules: TrackedSexpr,
    definition_env: &Env,
    captures: &mut Vec<Variable>,
) -> Result<TrackedSexpr> {
    if rules.is_null() {
        Ok(rules)
    } else if rules.is_pair() {
        let (rule, next) = rules.decons().unwrap();
        let (pattern, tail) = rule.decons().unwrap();
        let (template, _) = tail.decons().unwrap();

        let macro_vars = parse_pattern(ellipsis, literals, pattern.cdr().unwrap());

        let template = prepare_template(template, &macro_vars, definition_env, captures)?;

        let rule = TrackedSexpr::cons(
            pattern,
            TrackedSexpr::cons(template, TrackedSexpr::nil(NoSource), NoSource),
            NoSource,
        );

        let next = prepare_syntax_rules(ellipsis, literals, next, definition_env, captures)?;

        Ok(TrackedSexpr::cons(rule, next, NoSource))
    } else {
        Err(ObjectifyError {
            kind: ObjectifyErrorKind::SyntaxError,
            location: rules.src.clone(),
        })
    }
}

fn parse_pattern(
    ellipsis: Symbol,
    literals: &TrackedSexpr,
    rule: &TrackedSexpr,
) -> HashSet<Symbol> {
    use Sexpr::*;
    match &rule.sexpr {
        lit if literals.contains(lit) => HashSet::new(),
        Symbol(s) if *s == ellipsis => unimplemented!(),
        Symbol(s) => {
            let mut macro_var = HashSet::new();
            macro_var.insert(*s);
            macro_var
        }
        Pair(p) => {
            let mut a = parse_pattern(ellipsis, literals, &p.0);
            let b = parse_pattern(ellipsis, literals, &p.1);
            a.extend(b);
            a
        }
        Vector(_) => unimplemented!(),
        _ => HashSet::new(),
    }
}

fn prepare_template(
    template: TrackedSexpr,
    macro_vars: &HashSet<Symbol>,
    definition_env: &Env,
    captures: &mut Vec<Variable>,
) -> Result<TrackedSexpr> {
    use Sexpr::*;
    match template.sexpr {
        Symbol(s) if macro_vars.contains(&s) => Ok(template),
        Symbol(s) => Ok(definition_env
            .find_variable(&s)
            .map(|var| {
                if !captures.contains(&var) {
                    captures.push(var);
                }
                mark_captured_binding(template)
            })
            .unwrap_or_else(|| TrackedSexpr {
                sexpr: Symbol(s.as_uninterned()),
                src: NoSource,
            })),
        Pair(p) => Ok(TrackedSexpr::cons(
            prepare_template(p.0, macro_vars, definition_env, captures)?,
            prepare_template(p.1, macro_vars, definition_env, captures)?,
            NoSource,
        )),
        _ => Ok(template),
    }
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
            realize_template(template.clone(), &bound_vars, definition_env)
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
    definition_env: &Env,
) -> Result<TrackedSexpr> {
    use Sexpr::*;
    match &template.sexpr {
        _ if is_captured_binding(&template) => Ok(template),
        Symbol(s) => Ok(bound_vars.get(&s).cloned().unwrap_or(template)),
        Pair(_) => {
            let (car, cdr) = template.decons().unwrap();
            Ok(TrackedSexpr::cons(
                realize_template(car, bound_vars, definition_env)?,
                realize_template(cdr, bound_vars, definition_env)?,
                NoSource,
            ))
        }
        _ => Ok(template),
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
    _trans: &mut Translate,
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
