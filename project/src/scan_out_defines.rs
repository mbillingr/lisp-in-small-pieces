use crate::objectify::{ObjectifyError, ObjectifyErrorKind, Result};
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::source::SourceLocation;
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
        .map(|rv| rv.map(Clone::clone))
        .collect::<Result<_>>()?;

    if variables.is_empty() {
        return Ok(body);
    }

    let new_body_list: Vec<_> = body_list
        .iter()
        .cloned()
        .map(|expr| {
            if is_definition(&&expr) {
                Ok(make_assignment(
                    definition_variable(&expr)?.clone(),
                    definition_value(&expr)?.clone(),
                ))
            } else {
                Ok(expr)
            }
        })
        .collect::<Result<_>>()?;

    Ok(vec![make_let(variables, new_body_list)].into())
}

fn is_definition(expr: &&TrackedSexpr) -> bool {
    expr.is_list() && expr.at(0).map(|sx| sx == "define").unwrap_or(false)
}

pub fn definition_variable(expr: &TrackedSexpr) -> Result<&TrackedSexpr> {
    expr.at(1)
        .and_then(|var| {
            if var.is_symbol() {
                Some(var)
            } else {
                var.first()
            }
        })
        .ok_or_else(|| ObjectifyError {
            kind: ObjectifyErrorKind::ExpectedList,
            location: expr.source().clone(),
        })
}

pub fn definition_value(expr: &TrackedSexpr) -> Result<TrackedSexpr> {
    expr.at(1)
        .and_then(|var| {
            if var.is_symbol() {
                expr.at(2).cloned()
            } else {
                Some(make_function(
                    expr.at(1).unwrap().tail()?,
                    expr.tail().unwrap().tail()?,
                    expr.src.clone(),
                ))
            }
        })
        .ok_or_else(|| ObjectifyError {
            kind: ObjectifyErrorKind::ExpectedList,
            location: expr.source().clone(),
        })
}

fn make_assignment(variable: TrackedSexpr, value: TrackedSexpr) -> TrackedSexpr {
    vec![Sexpr::Symbol(Symbol::new("set!")).into(), variable, value].into()
}

pub fn make_function(
    variables: TrackedSexpr,
    body: TrackedSexpr,
    src: SourceLocation,
) -> TrackedSexpr {
    let mut func = TrackedSexpr::cons(
        Sexpr::Symbol(Symbol::new("lambda")).into(),
        TrackedSexpr::cons(variables, body),
    );
    func.src = src;
    func
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
