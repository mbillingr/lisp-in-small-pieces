use crate::objectify::{ObjectifyError, ObjectifyErrorKind, Result};
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::symbol::Symbol;

pub fn scan_out_defines(body: TrackedSexpr) -> Result<TrackedSexpr> {
    let body_list = body.as_proper_list().ok_or_else(|| ObjectifyError {
        kind: ObjectifyErrorKind::ExpectedList,
        location: body.source().clone(),
    })?;

    let variables: Vec<_> = body_list
        .iter()
        .filter(is_definition)
        .map(definition_variable)
        .collect();

    if variables.is_empty() {
        return Ok(body);
    }

    let new_body_list: Vec<_> = body_list
        .iter()
        .cloned()
        .map(|expr| {
            if is_definition(&&expr) {
                make_assignment(definition_variable(&expr), definition_value(expr))
            } else {
                expr
            }
        })
        .collect();

    Ok(vec![make_let(variables, new_body_list)].into())
}

fn is_definition(expr: &&TrackedSexpr) -> bool {
    expr.is_list() && expr.at(0).map(|sx| sx == "define").unwrap_or(false)
}

fn definition_variable(expr: &TrackedSexpr) -> TrackedSexpr {
    expr.at(1).unwrap().clone()
}

fn definition_value(expr: TrackedSexpr) -> TrackedSexpr {
    expr.at(2).unwrap().clone()
}

fn make_assignment(variable: TrackedSexpr, value: TrackedSexpr) -> TrackedSexpr {
    vec![Sexpr::Symbol(Symbol::new("set!")).into(), variable, value].into()
}

fn make_let(variables: Vec<TrackedSexpr>, body: Vec<TrackedSexpr>) -> TrackedSexpr {
    let mut func = vec![
        Sexpr::Symbol(Symbol::new("lambda")).into(),
        variables.into(),
    ];
    func.extend(body);

    let mut call = vec![func.into()];
    call.extend(vec![Sexpr::Uninitialized.into()]);

    call.into()
}
