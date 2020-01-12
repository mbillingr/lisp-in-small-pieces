use crate::env::Env;
use crate::objectify::Result;
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::symbol::Symbol;
use crate::syntax::MagicKeywordHandler;

pub fn eval_syntax(expr: &TrackedSexpr, env: &Env) -> Result<MagicKeywordHandler> {
    match expr.at(0) {
        Some(s) if s == "syntax-rules" => {
            if let Some(ellipsis) = expr.at(1).unwrap().as_symbol() {
                let literals = expr.at(2).unwrap();
                let rules = expr.cdr().unwrap().cdr().unwrap().cdr().unwrap();
                eval_syntax_rules(*ellipsis, literals, &rules, env)
            } else {
                let literals = expr.at(1).unwrap();
                let rules = expr.cdr().unwrap().cdr().unwrap();
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
    _env: &Env,
) -> Result<MagicKeywordHandler> {
    println!("{}", literals);
    /*for rule in &**rules.as_proper_list().unwrap() {
        let pattern = rule.at(0).unwrap();
        let template = rule.at(1).unwrap();

        let _bound_vars = extract_pattern_bindings(pattern, ellipsis, &literals);
        println!("rule: pattern={}, template={}", pattern, template);
    }*/
    unimplemented!()
}

fn extract_pattern_bindings(
    pattern: &TrackedSexpr,
    ellipsis: Symbol,
    literals: &[Symbol],
) -> Vec<Symbol> {
    match &pattern.sexpr {
        Sexpr::Symbol(s) if *s != ellipsis && !literals.contains(s) => vec![*s],
        Sexpr::Pair(p) => {
            let mut bindings = extract_pattern_bindings(&p.0, ellipsis, literals);
            bindings.extend(extract_pattern_bindings(&p.1, ellipsis, literals));
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
