use lisp_core::simple::Value as lcValue;
use lisp_core::{
    common::{BasicLispValue, CombinedLispOps, LispError},
    lexpr,
};
use rustyline::{error::ReadlineError, Editor};
use std::fmt::Display;
use std::str::FromStr;

pub fn read<T: From<lexpr::Value>>() -> Result<T> {
    let mut rl = Editor::<()>::new();
    Ok(lexpr::Value::from_str(&rl.readline(">> ")?)?.into())
}

pub fn is_atomic<T: BasicLispValue>(x: &T) -> bool {
    !x.is_pair()
}

pub fn is_self_evaluating<T>(x: &lcValue<T>) -> bool {
    use lcValue::*;
    match x {
        Nil | True | False | Int(_) | Float(_) => true,
        Undefined | Pair(_) | Symbol(_) | Function(_) => false,
    }
}

pub fn lookup<'a, T: CombinedLispOps + Display + PartialEq>(id: &T, env: &'a T) -> Result<&'a T> {
    if env.is_null() {
        Err(Error::UnboundVariable(format!("{}", id)))
    } else if env.caar()? == id {
        Ok(env.cdar()?)
    } else {
        lookup(id, env.cdr()?)
    }
}

pub fn update<T: CombinedLispOps + Display + PartialEq + Clone>(
    id: &T,
    env: &T,
    value: &T,
) -> Result<T> {
    if env.is_null() {
        Err(Error::UnboundVariable(format!("{}", id)))
    } else if env.caar()? == id {
        env.car()?.set_cdr(value.clone())?;
        Ok(value.clone())
    } else {
        update(id, env.cdr()?, value)
    }
}

pub fn extend<T: BasicLispValue + Clone>(env: &T, variables: &T, values: &T) -> Result<T> {
    if variables.is_pair() {
        if values.is_pair() {
            let entry = T::cons(variables.car()?.clone(), values.car()?.clone());
            Ok(T::cons(
                entry,
                extend(env, variables.cdr()?, values.cdr()?)?,
            ))
        } else {
            Err(Error::TooFewValues)
        }
    } else if variables.is_null() {
        if values.is_null() {
            Ok(env.clone())
        } else {
            Err(Error::TooManyValues)
        }
    } else if variables.is_symbol() {
        Ok(T::cons(
            T::cons(variables.clone(), values.clone()),
            env.clone(),
        ))
    } else {
        unreachable!()
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    UnboundVariable(String),
    TooFewValues,
    TooManyValues,
    EndOfFile,
    CoreError(LispError),
    ReadLineError(ReadlineError),
    ParserError(lexpr::parse::error::Error),
}

impl From<LispError> for Error {
    fn from(le: LispError) -> Error {
        Error::CoreError(le)
    }
}

impl From<ReadlineError> for Error {
    fn from(rle: ReadlineError) -> Error {
        match rle {
            ReadlineError::Eof => Error::EndOfFile,
            _ => Error::ReadLineError(rle),
        }
    }
}

impl From<lexpr::parse::error::Error> for Error {
    fn from(lpe: lexpr::parse::error::Error) -> Error {
        Error::ParserError(lpe)
    }
}
