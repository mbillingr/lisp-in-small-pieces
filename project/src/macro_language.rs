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
            if expr.at(1).unwrap().is_identifier() {
                let ellipsis = expr.at(1).unwrap();
                let literals = expr.at(2).unwrap();
                let rules = expr.cdr().unwrap().cdr().unwrap().cdr().unwrap();
                eval_syntax_rules(
                    name,
                    ellipsis.clone(),
                    literals.clone(),
                    rules.clone(),
                    env.clone(),
                )
            } else {
                let literals = expr.at(1).unwrap();
                let rules = expr.cdr().unwrap().cdr().unwrap();
                eval_syntax_rules(
                    name,
                    TrackedSexpr::symbol("...", NoSource),
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
    ellipsis: TrackedSexpr,
    literals: TrackedSexpr,
    rules: TrackedSexpr,
    definition_env: Env,
) -> Result<MagicKeywordHandler> {
    let rules = prepare_rules(&ellipsis, &literals, &rules, &definition_env)?;
    Ok(MagicKeywordHandler::new(
        move |trans: &mut Translate, expr: &TrackedSexpr| -> Result<Expression> {
            let sexpr = apply_syntax_rules(
                name,
                expr,
                &ellipsis,
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
    ellipsis: &TrackedSexpr,
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

    let mut unclosed = pattern_vars(ellipsis, literals, pattern.cdr()?);
    unclosed.insert(ellipsis.identifier_name().unwrap());
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
    //println!("{} ?", template);
    match &template.sexpr {
        Sexpr::SyntacticClosure(sc) if sc.sexpr().as_symbol().map(|s| unclosed.contains(s)).unwrap_or(false) => template.clone(),
        Sexpr::Symbol(s) if unclosed.contains(s) => template.clone(),
        Sexpr::Symbol(s) /*if definition_env.find_variable(s).is_some()*/ => aliases
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
    ellipsis: &TrackedSexpr,
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
            //println!("{} matched {}", expr, pattern);
            let result = match realize_template(&MultiIndex::new(), template, &bound_vars, ellipsis)
            {
                Ok(r) => r,
                Err(MultiIndexError::Fail(_)) => unreachable!(),
                Err(MultiIndexError::IndexTooShallow) => {
                    return Err(Error::at_expr(
                        ObjectifyErrorKind::MismatchedEllipses,
                        template,
                    ))
                }
            };
            //println!("=> {}", result);
            //Ok(Rc::new(SyntacticClosure::new(result, env.clone())).into())
            Ok(result)
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
        Err(Error::at_expr(
            ObjectifyErrorKind::SyntaxError(Box::leak(Box::new(format!(
                "no rule matched in {}",
                expr
            )))),
            expr,
        ))
    }
}

fn match_pattern(
    expr: &TrackedSexpr,
    ellipsis: &TrackedSexpr,
    literals: &TrackedSexpr,
    pattern: &TrackedSexpr,
) -> Option<HashMap<Symbol, Binding>> {
    //println!("matching {} and {}", expr, pattern);
    use Sexpr::*;
    match (&pattern.sexpr, &expr.sexpr) {
        (SyntacticClosure(sc), _) => match_pattern(expr, ellipsis, literals, sc.sexpr()),
        (lit, x) if literals.contains(lit) => {
            if lit == x {
                Some(HashMap::new())
            } else {
                None
            }
        }
        (Symbol(s), _) if s == "_" => Some(HashMap::new()),
        (Symbol(s), _) => {
            let mut binding = HashMap::new();
            binding.insert(*s, Binding::One(expr.clone()));
            Some(binding)
        }
        (Pair(p), _) => {
            if p.1
                .car()
                .map(|next| {
                    next.is_identifier()
                        && ellipsis.is_identifier()
                        && next.identifier_name() == ellipsis.identifier_name()
                })
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
        _ if pattern == expr => Some(HashMap::new()),
        _ => None,
    }
}

fn pattern_vars(
    ellipsis: &TrackedSexpr,
    literals: &TrackedSexpr,
    pattern: &TrackedSexpr,
) -> HashSet<Symbol> {
    use Sexpr::*;
    match &pattern.sexpr {
        lit if literals.contains(lit) => HashSet::new(),
        _ if pattern.identifier_name() == ellipsis.identifier_name() => HashSet::new(),
        _ if pattern.is_identifier() => match pattern.identifier_name().unwrap() {
            s if &s == "_" => HashSet::new(),
            s => {
                let mut vars = HashSet::new();
                vars.insert(s);
                vars
            }
        },
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
    idx: &MultiIndex,
    template: &TrackedSexpr,
    bound_vars: &HashMap<Symbol, Binding>,
    ellipsis: &TrackedSexpr,
) -> MultiIndexResult<TrackedSexpr> {
    use Sexpr::*;
    //println!("realizing {}", template);
    match &template.sexpr {
        Pair(_) => {
            let car = template.car().unwrap();
            let cdr = template.cdr().unwrap();
            if car.identifier_name() == ellipsis.identifier_name() {
                realize_template(
                    idx,
                    cdr.car().unwrap(),
                    bound_vars,
                    &TrackedSexpr::symbol(crate::symbol::Symbol::uninterned(""), NoSource),
                )
            } else {
                if cdr
                    .car()
                    .map(|next| next.identifier_name() == ellipsis.identifier_name())
                    .unwrap_or(false)
                {
                    let rep = realize_repeated_template(idx, &car, bound_vars, ellipsis)?;
                    let mut rest = realize_template(idx, cdr.cdr().unwrap(), bound_vars, ellipsis)?;
                    for r in rep.into_iter().rev() {
                        rest = TrackedSexpr::cons(r, rest, NoSource);
                    }
                    Ok(rest)
                } else {
                    Ok(TrackedSexpr::cons(
                        realize_template(idx, &car, bound_vars, ellipsis)?,
                        realize_template(idx, &cdr, bound_vars, ellipsis)?,
                        NoSource,
                    ))
                }
            }
        }
        _ if template.is_identifier() => {
            let s = template.identifier_name().unwrap();
            match bound_vars.get(&s) {
                None => Ok(template.clone()),
                Some(binding) => binding.get(idx.as_ref()).map(|x| x.clone()),
            }
        }
        _ => Ok(template.clone()),
    }
}

fn realize_repeated_template(
    idx: &MultiIndex,
    template: &TrackedSexpr,
    bound_vars: &HashMap<Symbol, Binding>,
    ellipsis: &TrackedSexpr,
) -> MultiIndexResult<Vec<TrackedSexpr>> {
    use MultiIndexError::*;
    let mut idx = idx.next_level();
    let mut result = vec![];
    loop {
        match realize_template(&idx, template, bound_vars, ellipsis) {
            Err(Fail(level)) if level < idx.len() - 1 => return Err(Fail(level)),
            Err(Fail(_)) => return Ok(result),
            Err(e) => return Err(e),
            Ok(r) => {
                result.push(r);
            }
        }
        idx = idx.next();
    }
}

#[derive(Debug, Clone)]
enum Binding {
    One(TrackedSexpr),
    Sequence(Vec<Binding>),
}

impl Binding {
    pub fn get(&self, index: &[usize]) -> MultiIndexResult<&TrackedSexpr> {
        use MultiIndexError::*;
        match self {
            Binding::One(sexpr) => Ok(sexpr),
            Binding::Sequence(seq) => {
                if index.is_empty() {
                    return Err(IndexTooShallow);
                }
                match seq.get(index[0]) {
                    Some(b) => match b.get(&index[1..]) {
                        Ok(x) => Ok(x),
                        Err(Fail(level)) => Err(Fail(level + 1)),
                        Err(e) => Err(e),
                    },
                    None => Err(Fail(0)),
                }
            }
        }
    }
}

#[derive(Debug)]
struct MultiIndex {
    index: Vec<usize>,
}

impl MultiIndex {
    pub fn new() -> Self {
        MultiIndex { index: vec![] }
    }

    pub fn len(&self) -> usize {
        self.index.len()
    }

    pub fn next_level(&self) -> Self {
        let mut index = self.index.clone();
        index.push(0);
        MultiIndex { index }
    }

    pub fn next(&self) -> Self {
        let mut index = self.index.clone();
        *index.last_mut().unwrap() += 1;
        MultiIndex { index }
    }
}

impl AsRef<[usize]> for MultiIndex {
    fn as_ref(&self) -> &[usize] {
        self.index.as_slice()
    }
}

type MultiIndexResult<T> = std::result::Result<T, MultiIndexError>;

enum MultiIndexError {
    Fail(usize),
    IndexTooShallow,
}
