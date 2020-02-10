use crate::env::Env;
use crate::error::{Error, Result};
use crate::objectify::{ObjectifyErrorKind, Translate};
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::source::SourceLocation::NoSource;
use crate::symbol::Symbol;
use crate::syntactic_closure::SyntacticClosure;
use crate::syntax::{Expression, MagicKeywordHandler};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub fn eval_syntax(expr: &TrackedSexpr, env: &Env) -> Result<MagicKeywordHandler> {
    let name = *expr.car()?.as_symbol()?;
    let expr = expr.cdr()?.car()?;
    match expr.at(0) {
        Ok(s) if s == "syntax-rules" => {
            if let Ok(ellipsis) = expr.at(1).unwrap().as_symbol() {
                let literals = expr.at(2).unwrap();
                let rules = expr.cdr().unwrap().cdr().unwrap().cdr().unwrap();
                eval_syntax_rules(
                    name,
                    *ellipsis,
                    literals.clone(),
                    rules.clone(),
                    env.clone(),
                )
            } else {
                let literals = expr.at(1).unwrap();
                let rules = expr.cdr().unwrap().cdr().unwrap();
                eval_syntax_rules(
                    name,
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
    name: Symbol,
    ellipsis: Symbol,
    literals: TrackedSexpr,
    rules: TrackedSexpr,
    definition_env: Env,
) -> Result<MagicKeywordHandler> {
    let rules = prepare_rules(ellipsis, &literals, &rules, &definition_env)?;
    Ok(Rc::new(
        move |trans: &mut Translate, expr: &TrackedSexpr| -> Result<Expression> {
            let sexpr = apply_syntax_rules(
                name,
                expr,
                ellipsis,
                &literals,
                &rules,
                &trans.env,
                &definition_env,
            )?;
            //println!("{} => {}", expr, sexpr);
            trans.objectify(&sexpr)
        },
    ))
}

fn prepare_rules(
    ellipsis: Symbol,
    literals: &TrackedSexpr,
    rules: &TrackedSexpr,
    definition_env: &Env,
) -> Result<TrackedSexpr> {
    if rules.is_null() {
        return Ok(rules.clone());
    }

    let rule = rules.car()?;
    let pattern = rule.at(0)?;
    let template = rule.at(1)?;

    let mut unclosed = pattern_vars(ellipsis, literals, pattern);
    unclosed.insert(ellipsis);
    literals.scan(|lit| {
        lit.as_symbol().and_then(|s| {
            unclosed.insert(*s);
            Ok(())
        })
    })?;

    let mut aliases = HashMap::new();

    let pattern = pattern.clone();
    let template = close_template_symbols(template, &unclosed, &mut aliases, definition_env);

    let rule = TrackedSexpr::list(vec![pattern, template], rule.src.clone());
    Ok(TrackedSexpr::cons(
        rule,
        prepare_rules(ellipsis, literals, rules.cdr().unwrap(), definition_env)?,
        rules.src.clone(),
    ))
}

fn close_template_symbols(
    template: &TrackedSexpr,
    unclosed: &HashSet<Symbol>,
    aliases: &mut HashMap<Symbol, Rc<SyntacticClosure>>,
    definition_env: &Env,
) -> TrackedSexpr {
    match &template.sexpr {
        Sexpr::Symbol(s) if unclosed.contains(s) => template.clone(),
        Sexpr::Symbol(s) => aliases
            .entry(*s)
            .or_insert_with(|| {
                Rc::new(SyntacticClosure::new(
                    template.clone(),
                    definition_env.clone(),
                ))
            })
            .clone()
            .into(),
        Sexpr::Pair(p) => TrackedSexpr::cons(
            close_template_symbols(&p.0, unclosed, aliases, definition_env),
            close_template_symbols(&p.1, unclosed, aliases, definition_env),
            template.src.clone(),
        ),
        Sexpr::Vector(_) => unimplemented!(),
        _ => template.clone(),
    }
}

fn apply_syntax_rules(
    name: Symbol,
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
                //.map(|(name, x)| (name, x.into_syntactic_closure(env)))
                .collect();
            let result = realize_template(template.clone(), &bound_vars, ellipsis)?;
            Ok(Rc::new(SyntacticClosure::new(result, env.clone())).into())
        } else {
            apply_syntax_rules(
                name,
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

#[derive(Debug, Clone)]
enum Binding {
    One(TrackedSexpr),
    Sequence(Vec<Binding>),
}

fn match_pattern(
    expr: &TrackedSexpr,
    ellipsis: Symbol,
    literals: &TrackedSexpr,
    pattern: &TrackedSexpr,
) -> Option<HashMap<Symbol, Binding>> {
    //println!("matching {} and {}", expr, rule);
    use Sexpr::*;
    match (&pattern.sexpr, &expr.sexpr) {
        (lit, x) if literals.contains(lit) => {
            if lit == x {
                Some(HashMap::new())
            } else {
                None
            }
        }
        (Symbol(s), _) if s == "_" => Some(HashMap::new()),
        (Symbol(s), _) if *s == ellipsis => unreachable!(),
        (Symbol(s), _) => {
            let mut binding = HashMap::new();
            binding.insert(*s, Binding::One(expr.clone()));
            Some(binding)
        }
        (Pair(p), _) => {
            if p.1
                .car()
                .map(|next| next.sexpr == Symbol(ellipsis))
                .unwrap_or(false)
            {
                let n_after_ellipsis = p.1.cdr().unwrap().list_len();
                let n_expr = expr.list_len();
                let n_ellipsis = n_expr - n_after_ellipsis;

                let mut matches = vec![];
                let mut xp = expr;
                for _ in 0..n_ellipsis {
                    let x = xp.car().ok()?;
                    let m = match_pattern(&x, ellipsis, literals, &p.0)?;
                    matches.push(m);
                    xp = xp.cdr().unwrap();
                }
                let mut bindings: HashMap<_, _> = pattern_vars(ellipsis, literals, &p.0)
                    .into_iter()
                    .map(|name| (name, vec![]))
                    .collect();
                for m in matches {
                    for (k, b) in m {
                        bindings.entry(k).or_insert(vec![]).push(b);
                    }
                }
                let mut bindings: HashMap<_, _> = bindings
                    .into_iter()
                    .map(|(k, v)| (k, Binding::Sequence(v)))
                    .collect();
                let more = match_pattern(xp, ellipsis, literals, p.1.cdr().unwrap())?;
                bindings.extend(more);
                Some(bindings)
            } else {
                if let Pair(x) = &expr.sexpr {
                    let mut a = match_pattern(&x.0, ellipsis, literals, &p.0)?;
                    let b = match_pattern(&x.1, ellipsis, literals, &p.1)?;
                    a.extend(b);
                    Some(a)
                } else {
                    None
                }
            }
        }
        (Vector(_), Vector(_)) => unimplemented!(),
        /*(Pair(r), Pair(x)) => {
            let rv = vec![&r.0];
            let xv = vec![&x.0];
            match_sequence(&xv, Some(&x.1), ellipsis, literals, &rv, Some(&r.1))
        }
        (Vector(v), Vector(x)) => match_sequence(x, None, ellipsis, literals, v, None),*/
        _ if pattern == expr => Some(HashMap::new()),
        _ => None,
    }
}

fn pattern_vars(
    ellipsis: Symbol,
    literals: &TrackedSexpr,
    pattern: &TrackedSexpr,
) -> HashSet<Symbol> {
    use Sexpr::*;
    match &pattern.sexpr {
        lit if literals.contains(lit) => HashSet::new(),
        Symbol(s) if s == "_" => HashSet::new(),
        Symbol(s) if *s == ellipsis => HashSet::new(),
        Symbol(s) => {
            let mut vars = HashSet::new();
            vars.insert(*s);
            vars
        }
        Pair(p) => {
            let mut a = pattern_vars(ellipsis, literals, &p.0);
            let b = pattern_vars(ellipsis, literals, &p.1);
            a.extend(b);
            a
        }
        Vector(_) => unimplemented!(),
        _ => HashSet::new(),
    }
}

fn realize_template(
    template: TrackedSexpr,
    bound_vars: &HashMap<Symbol, Binding>,
    ellipsis: Symbol,
) -> Result<TrackedSexpr> {
    use Sexpr::*;
    match &template.sexpr {
        Pair(_) => {
            let (car, cdr) = template.decons().unwrap();
            if cdr.car().and_then(|x| x.as_symbol()).ok() == Some(&ellipsis) {
                let rep = realize_repeated_template(0, &car, bound_vars, ellipsis)?;
                let mut rest = realize_template(cdr.cdr().unwrap().clone(), bound_vars, ellipsis)?;
                for r in rep.into_iter().rev() {
                    rest = TrackedSexpr::cons(r, rest, NoSource);
                }
                Ok(rest)
            } else {
                Ok(TrackedSexpr::cons(
                    realize_template(car, bound_vars, ellipsis)?,
                    realize_template(cdr, bound_vars, ellipsis)?,
                    NoSource,
                ))
            }
        }
        Symbol(s) => Ok(match bound_vars.get(&s) {
            None => template,
            Some(Binding::One(x)) => x.clone(),
            Some(Binding::Sequence(_)) => unimplemented!(),
        }),
        _ => Ok(template),
    }
}

fn realize_repeated_template(
    mut nth: usize,
    template: &TrackedSexpr,
    bound_vars: &HashMap<Symbol, Binding>,
    ellipsis: Symbol,
) -> Result<Vec<TrackedSexpr>> {
    let mut result = vec![];
    loop {
        match realize_indexed_template(nth, template, bound_vars, ellipsis)? {
            None => return Ok(result),
            Some(r) => result.push(r),
        }
        nth += 1;
    }
}

fn realize_indexed_template(
    idx: usize,
    template: &TrackedSexpr,
    bound_vars: &HashMap<Symbol, Binding>,
    ellipsis: Symbol,
) -> Result<Option<TrackedSexpr>> {
    use Sexpr::*;
    match &template.sexpr {
        Pair(_) => {
            let car = template.car().unwrap();
            let cdr = template.cdr().unwrap();
            let a = realize_indexed_template(idx, car, bound_vars, ellipsis)?;
            let d = realize_indexed_template(idx, cdr, bound_vars, ellipsis)?;

            Ok(match (a, d) {
                (Some(a), Some(d)) => Some(TrackedSexpr::cons(a, d, NoSource)),
                _ => None,
            })
        }
        Symbol(s) => Ok(match bound_vars.get(&s) {
            None => Some(template.clone()),
            Some(Binding::One(x)) => Some(x.clone()),
            Some(Binding::Sequence(v)) => match v.get(idx) {
                None => None,
                Some(Binding::One(x)) => Some(x.clone()),
                Some(Binding::Sequence(_)) => unimplemented!("nested ellipses"),
            },
        }),
        _ => Ok(Some(template.clone())),
    }
}
