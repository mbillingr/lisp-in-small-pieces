use crate::objectify::{ObjectifyError, ObjectifyErrorKind};
use crate::parsing::{ParseError, ParseErrorKind};
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
    Parse(ParseErrorKind),
    Objectify(ObjectifyErrorKind),
    Compile(CompileError),
    Runtime(RuntimeError),
    TypeError(TypeError),
}

#[derive(Debug, PartialEq)]
pub enum CompileError {
    MacroUsedAsValue(Symbol),
}

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    ValueStackUnderflow,
    IncorrectArity,
    UndefinedGlobal(Symbol),
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    WrongType,
    NotCallable,
    NoPair,
    NoSymbol,
}

#[derive(Debug)]
pub enum ErrorContext {
    None,
    Source(SourceLocation),
}

impl From<ObjectifyError> for Error {
    fn from(err: ObjectifyError) -> Self {
        Error {
            kind: ErrorKind::Objectify(err.kind),
            context: ErrorContext::Source(err.location),
        }
    }
}

impl From<RuntimeError> for Error {
    fn from(err: RuntimeError) -> Self {
        Error {
            kind: ErrorKind::Runtime(err),
            context: ErrorContext::None,
        }
    }
}

impl From<TypeError> for Error {
    fn from(err: TypeError) -> Self {
        Error {
            kind: ErrorKind::TypeError(err),
            context: ErrorContext::None,
        }
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
            ErrorKind::Parse(e) => write!(f, "Parse Error: {:?}", e),
            ErrorKind::Objectify(e) => write!(f, "Syntax Error: {:?}", e),
            ErrorKind::Runtime(e) => write!(f, "Runtime Error: {:?}", e),
            ErrorKind::Compile(e) => write!(f, "Compile Error: {:?}", e),
            ErrorKind::TypeError(e) => write!(f, "Type Error: {:?}", e),
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
