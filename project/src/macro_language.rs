use crate::env::Env;
use crate::objectify::Result;
use crate::sexpr::{TrackedSexpr, Sexpr};
use crate::symbol::Symbol;
use crate::syntax::MagicKeywordHandler;

pub fn eval_syntax(expr: &TrackedSexpr, env: &Env) -> Result<MagicKeywordHandler> {
    match expr.at(0) {
        Some(s) if s == "syntax-rules" => {
            if let Some(ellipsis) = expr.at(1).unwrap().as_symbol() {
                let literals = expr.at(2).unwrap();
                let rules = expr.tail().unwrap().tail().unwrap().tail().unwrap();
                eval_syntax_rules(*ellipsis, literals, &rules, env)
            } else {
                let literals = expr.at(1).unwrap();
                let rules = expr.tail().unwrap().tail().unwrap();
                eval_syntax_rules(Symbol::new("..."), literals, &rules, env)
            }
        }
        _ => panic!("Syntax error in macro definition: {}", expr),
    }
}

pub fn eval_syntax_rules(
    ellipsis: Symbol,
    literals: &TrackedSexpr,
    rules: &TrackedSexpr,
    env: &Env,
) -> Result<MagicKeywordHandler> {
    println!("{}", literals);
    let literals: Vec<_> = literals
        .as_proper_list()
        .unwrap()
        .iter()
        .map(|s| *s.as_symbol().unwrap())
        .collect();

    for rule in &**rules.as_proper_list().unwrap() {
        let pattern = rule.at(0).unwrap();
        let template = rule.at(1).unwrap();

        let bound_vars = extract_pattern_bindings(pattern, ellipsis, &literals);
        println!("rule: pattern={}, template={}", pattern, template);
    }
    unimplemented!()
}

fn extract_pattern_bindings(pattern: &TrackedSexpr, ellipsis: Symbol, literals: &[Symbol]) -> Vec<Symbol> {
    match &pattern.sexpr {
        Sexpr::Symbol(s) if *s != ellipsis && !literals.contains(s) => vec![*s],
        Sexpr::List(l, d) => {
            let mut bindings = vec![];
            for subpat in l.iter() {
                bindings.extend(extract_pattern_bindings(subpat, ellipsis, literals));
            }
            if let Some(subpat) = d {
                bindings.extend(extract_pattern_bindings(subpat, ellipsis, literals));
            }
            bindings
        }
        Sexpr::Vector(v) => {
            let mut bindings = vec![];
            for subpat in v.iter() {
                bindings.extend(extract_pattern_bindings(subpat, ellipsis, literals));
            }
            bindings
        }
        _ => vec![],
    }
}
