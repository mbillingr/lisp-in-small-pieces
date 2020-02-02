use crate::error::{Error, Result};
use crate::objectify::ObjectifyErrorKind;
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use SourceLocation::NoSource;

pub fn scan_out_defines(body: TrackedSexpr) -> Result<TrackedSexpr> {
    let uninit: TrackedSexpr = Sexpr::Uninitialized.into();

    let mut variables = TrackedSexpr::nil(NoSource);
    let mut values = TrackedSexpr::nil(NoSource);
    body.scan(|expr| -> Result<()> {
        if is_definition(expr) {
            let vars = std::mem::replace(&mut variables, TrackedSexpr::nil(NoSource));
            variables = TrackedSexpr::cons(definition_variable(expr)?.clone(), vars, NoSource);
            values = TrackedSexpr::cons(uninit.clone(), TrackedSexpr::nil(NoSource), NoSource);
        }
        Ok(())
    })?;

    if variables.is_null() {
        return Ok(body);
    }

    fn transform(body: TrackedSexpr) -> Result<TrackedSexpr> {
        match body.decons() {
            Ok((mut expr, rest)) => {
                let src = rest.src.start_at(&expr.src);
                if is_definition(&expr) {
                    expr = make_assignment(
                        definition_variable(&expr)?.clone(),
                        definition_value(&expr)?.clone(),
                    )
                }
                transform(rest)
                    .map(|transformed_rest| TrackedSexpr::cons(expr, transformed_rest, src))
            }
            Err(body) => Ok(body),
        }
    }

    let new_body = make_let(variables, values, transform(body)?);
    Ok(TrackedSexpr::cons(
        new_body,
        TrackedSexpr::nil(NoSource),
        NoSource,
    ))
}

fn is_definition(expr: &TrackedSexpr) -> bool {
    expr.at(0).map(|sx| sx == "define").unwrap_or(false)
}

pub fn definition_variable(expr: &TrackedSexpr) -> Result<&TrackedSexpr> {
    expr.at(1)
        .and_then(|var| {
            if var.is_symbol() {
                Some(var)
            } else {
                var.car()
            }
        })
        .ok_or_else(|| Error::at_expr(ObjectifyErrorKind::ExpectedList, expr))
}

pub fn definition_value(expr: &TrackedSexpr) -> Result<TrackedSexpr> {
    expr.at(1)
        .and_then(|var| {
            if var.is_symbol() {
                expr.at(2).cloned()
            } else {
                Some(make_function(
                    expr.at(1).unwrap().cdr()?.clone(),
                    expr.cdr().unwrap().cdr()?.clone(),
                    expr.src.clone(),
                ))
            }
        })
        .ok_or_else(|| Error::at_expr(ObjectifyErrorKind::ExpectedList, expr))
}

fn make_assignment(variable: TrackedSexpr, value: TrackedSexpr) -> TrackedSexpr {
    use TrackedSexpr as S;
    S::cons(
        Sexpr::Symbol(Symbol::new("set!")).into(),
        S::cons(
            variable,
            S::cons(value, TrackedSexpr::nil(NoSource), NoSource),
            NoSource,
        ),
        NoSource,
    )
}

pub fn make_function(
    variables: TrackedSexpr,
    body: TrackedSexpr,
    src: SourceLocation,
) -> TrackedSexpr {
    let part_src = body.src.start_at(&variables.src);
    TrackedSexpr::cons(
        Sexpr::Symbol(Symbol::new("lambda")).into(),
        TrackedSexpr::cons(variables, body, part_src),
        src,
    )
}

fn make_let(variables: TrackedSexpr, values: TrackedSexpr, body: TrackedSexpr) -> TrackedSexpr {
    let var_and_body = TrackedSexpr::cons(variables, body, NoSource);

    let func = TrackedSexpr::cons(
        Sexpr::Symbol(Symbol::new("lambda")).into(),
        var_and_body,
        NoSource,
    );

    let call = TrackedSexpr::cons(func, values, NoSource);

    call
}
