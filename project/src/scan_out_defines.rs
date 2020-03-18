use crate::error::{Error, Result};
use crate::objectify::ObjectifyErrorKind;
use crate::sexpr::{Sexpr, TrackedSexpr};
use crate::source::SourceLocation;
use crate::symbol::Symbol;
use SourceLocation::NoSource;

pub fn scan_out_defines(body: TrackedSexpr) -> Result<TrackedSexpr> {
    let uninit: TrackedSexpr = Sexpr::Uninitialized.into();

    let mut variables = TrackedSexpr::nil();
    let mut values = TrackedSexpr::nil();
    body.scan(|expr| -> Result<()> {
        if is_definition(expr) {
            let vars = std::mem::replace(&mut variables, TrackedSexpr::nil());
            variables = TrackedSexpr::cons(definition_variable(expr)?.clone(), vars);
            values = TrackedSexpr::cons(uninit.clone(), values.clone());
        }
        Ok(())
    })?;

    if variables.is_nil() {
        return Ok(body);
    }

    fn transform(body: TrackedSexpr) -> Result<TrackedSexpr> {
        match body.decons() {
            Ok((mut expr, rest)) => {
                let src = rest.source().start_at(&expr.source());
                if is_definition(&expr) {
                    expr = make_assignment(
                        definition_variable(&expr)?.clone(),
                        definition_value(&expr)?.clone(),
                    )
                }
                transform(rest)
                    .map(|transformed_rest| TrackedSexpr::cons(expr, transformed_rest).with_src(src))
            }
            Err(body) => Ok(body),
        }
    }

    let new_body = make_let(variables, values, transform(body)?);
    Ok(TrackedSexpr::cons(
        new_body,
        TrackedSexpr::nil()
    ))
}

fn is_definition(expr: &TrackedSexpr) -> bool {
    expr.at(0).map(|sx| sx == "define").unwrap_or(false)
}

pub fn definition_variable(expr: &TrackedSexpr) -> Result<TrackedSexpr> {
    expr.at(1)
        .and_then(|var| if var.is_symbol() { Ok(*var) } else { var.car() })
        .map_err(|_| Error::at_expr(ObjectifyErrorKind::ExpectedList, expr))
}

pub fn definition_value(expr: &TrackedSexpr) -> Result<TrackedSexpr> {
    expr.at(1)
        .and_then(|var| {
            if var.is_symbol() {
                expr.at(2).map(|x| x.clone())
            } else {
                Ok(make_function(
                    expr.at(1).unwrap().cdr()?.clone(),
                    expr.cdr().unwrap().cdr()?.clone(),
                    expr.source().clone(),
                ))
            }
        })
        .map_err(|_| Error::at_expr(ObjectifyErrorKind::ExpectedList, expr))
}

fn make_assignment(variable: TrackedSexpr, value: TrackedSexpr) -> TrackedSexpr {
    use TrackedSexpr as S;
    S::cons(
        Sexpr::Symbol(Symbol::new("set!")).into(),
        S::cons(
            variable,
            S::cons(value, TrackedSexpr::nil()),
        ),
    )
}

pub fn make_function(
    variables: TrackedSexpr,
    body: TrackedSexpr,
    src: SourceLocation,
) -> TrackedSexpr {
    let part_src = body.source().start_at(&variables.source());
    TrackedSexpr::cons(
        Sexpr::Symbol(Symbol::new("lambda")).into(),
        TrackedSexpr::cons(variables, body).with_src(part_src))
        .with_src(src)
}

fn make_let(variables: TrackedSexpr, values: TrackedSexpr, body: TrackedSexpr) -> TrackedSexpr {
    let var_and_body = TrackedSexpr::cons(variables, body);

    let func = TrackedSexpr::cons(
        Sexpr::Symbol(Symbol::new("lambda")).into(),
        var_and_body,
    );

    let call = TrackedSexpr::cons(func, values);

    call
}
