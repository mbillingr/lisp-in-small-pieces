use crate::objectify::ObjectifyErrorKind;
use crate::parsing::{ParseError, ParseErrorKind};
use crate::scm::Scm;
use crate::sexpr::TrackedSexpr;
use crate::source::{Source, SourceLocation};
use crate::symbol::Symbol;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub context: ErrorContext,
}

#[derive(Debug)]
pub enum ErrorKind {
    Custom(Scm),
    Parse(ParseErrorKind),
    Objectify(ObjectifyErrorKind),
    Compile(CompileError),
    Runtime(RuntimeError),
    TypeError(TypeError),
    IoError(std::io::Error),
}

#[derive(Debug, PartialEq)]
pub enum CompileError {
    MacroUsedAsValue(Symbol),
}

#[derive(PartialEq)]
pub enum RuntimeError {
    ValueStackUnderflow,
    IncorrectArity,
    UndefinedGlobal(Scm),
    InvalidExitProcedure,
}

impl std::fmt::Debug for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RuntimeError::ValueStackUnderflow => write!(f, "value-stack underflow"),
            RuntimeError::IncorrectArity => write!(f, "incorrect arity"),
            RuntimeError::UndefinedGlobal(name) => write!(f, "undefined global {}", name),
            RuntimeError::InvalidExitProcedure => write!(f, "invalid exit procedure"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    WrongType,
    NotCallable(Scm),
    NoInt,
    NoPositiveInt(Scm),
    NoPair(Scm),
    NoVector,
    NoSymbol,
    NoString(Scm),
    NoClosure,
    OutOfBounds,
}

#[derive(Debug)]
pub enum ErrorContext {
    None,
    Source(SourceLocation),
}

impl Error {
    pub fn at_expr(kind: impl Into<ErrorKind>, expr: &TrackedSexpr) -> Self {
        Error::at_span(kind, expr.source().clone())
    }

    pub fn at_span(kind: impl Into<ErrorKind>, span: SourceLocation) -> Self {
        Error {
            kind: kind.into(),
            context: ErrorContext::Source(span),
        }
    }
}

impl<T> From<T> for Error
where
    ErrorKind: From<T>,
{
    fn from(kind: T) -> Self {
        Error {
            kind: kind.into(),
            context: ErrorContext::None,
        }
    }
}

impl From<std::convert::Infallible> for Error {
    fn from(_: std::convert::Infallible) -> Self {
        unreachable!()
    }
}

impl From<RuntimeError> for ErrorKind {
    fn from(err: RuntimeError) -> Self {
        ErrorKind::Runtime(err)
    }
}

impl From<TypeError> for ErrorKind {
    fn from(err: TypeError) -> Self {
        ErrorKind::TypeError(err)
    }
}

impl From<ObjectifyErrorKind> for ErrorKind {
    fn from(err: ObjectifyErrorKind) -> Self {
        ErrorKind::Objectify(err)
    }
}

impl From<std::io::Error> for ErrorKind {
    fn from(err: std::io::Error) -> Self {
        ErrorKind::IoError(err)
    }
}

impl Error {
    pub fn from_parse_error_and_source(err: ParseError, src: Source) -> Error {
        assert_eq!(err.location.text, &*src.content);
        Error {
            kind: ErrorKind::Parse(err.kind),
            context: ErrorContext::Source(src.loc(err.location.start, err.location.end)),
        }
    }
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ErrorKind::Custom(e) => write!(f, "Error: {}", e),
            ErrorKind::Parse(e) => write!(f, "Parse Error: {:?}", e),
            ErrorKind::Objectify(e) => write!(f, "Syntax Error: {:?}", e),
            ErrorKind::Runtime(e) => write!(f, "Runtime Error: {:?}", e),
            ErrorKind::Compile(e) => write!(f, "Compile Error: {:?}", e),
            ErrorKind::TypeError(e) => write!(f, "Type Error: {:?}", e),
            ErrorKind::IoError(e) => write!(f, "I/O Error: {:?}", e),
        }
    }
}

impl PartialEq<ObjectifyErrorKind> for Error {
    fn eq(&self, other: &ObjectifyErrorKind) -> bool {
        match self.kind {
            ErrorKind::Objectify(ref e) => e == other,
            _ => false,
        }
    }
}

impl PartialEq<Error> for ObjectifyErrorKind {
    fn eq(&self, other: &Error) -> bool {
        other.eq(self)
    }
}

impl PartialEq<ParseErrorKind> for Error {
    fn eq(&self, other: &ParseErrorKind) -> bool {
        match self.kind {
            ErrorKind::Parse(ref e) => e == other,
            _ => false,
        }
    }
}

impl PartialEq<Error> for ParseErrorKind {
    fn eq(&self, other: &Error) -> bool {
        other.eq(self)
    }
}

impl PartialEq<RuntimeError> for Error {
    fn eq(&self, other: &RuntimeError) -> bool {
        match self.kind {
            ErrorKind::Runtime(ref e) => e == other,
            _ => false,
        }
    }
}

impl PartialEq<Error> for RuntimeError {
    fn eq(&self, other: &Error) -> bool {
        other.eq(self)
    }
}

impl PartialEq<TypeError> for Error {
    fn eq(&self, other: &TypeError) -> bool {
        match self.kind {
            ErrorKind::TypeError(ref e) => e == other,
            _ => false,
        }
    }
}

impl PartialEq<Error> for TypeError {
    fn eq(&self, other: &Error) -> bool {
        other.eq(self)
    }
}
