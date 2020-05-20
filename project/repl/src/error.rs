use crate::objectify::ObjectifyErrorKind;
use crate::scm::Scm;
use crate::sexpr::TrackedSexpr;
use std::fmt::{Display, Formatter};
use sunny_common::{Source, SourceLocation, Symbol};
use sunny_parser::ParseError;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    pub error: Box<dyn std::error::Error>,
    pub context: ErrorContext,
}

impl Error {
    pub fn new(e: impl std::error::Error + 'static, context: ErrorContext) -> Self {
        Error {
            error: Box::new(e),
            context,
        }
    }

    pub fn chain(self, other: impl std::error::Error + 'static) -> Self {
        Error {
            error: Box::new(ChainedError {
                first: Box::new(self),
                next: Box::new(other),
            }),
            context: ErrorContext::None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.error)
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub struct ChainedError {
    pub first: Box<dyn std::error::Error>,
    pub next: Box<dyn std::error::Error>,
}

impl Display for ChainedError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} --> {}", self.first, self.next)
    }
}

impl std::error::Error for ChainedError {}

/*#[derive(Debug)]
pub enum ErrorKind {
    Custom(Scm),
    Parse(ParseErrorKind),
    Objectify(ObjectifyErrorKind),
    Compile(CompileError),
    Runtime(RuntimeError),
    TypeError(TypeError),
    IoError(std::io::Error),
    Utf8Error(std::str::Utf8Error),

    Chained(Box<Error>, Box<Error>),
    Unhandled(Scm),
}

impl std::error::Error for ErrorKind { }*/

#[derive(Debug)]
pub struct UnhandledException {
    obj: Scm,
}

impl UnhandledException {
    pub fn new(obj: Scm) -> Self {
        UnhandledException { obj }
    }

    pub fn obj(&self) -> Scm {
        self.obj
    }
}

impl Display for UnhandledException {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "Unhandled exception: {}", self.obj.write())
    }
}

impl std::error::Error for UnhandledException {}

#[derive(Debug, PartialEq)]
pub enum CompileError {
    MacroUsedAsValue(Symbol),
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for CompileError {}

#[derive(PartialEq)]
pub enum RuntimeError {
    ValueStackUnderflow,
    IncorrectArity,
    UndefinedGlobal(Scm),
    InvalidExitProcedure,
    ClosedPort,
    WrongPortKind,
    WriteError,
    IndexOutOfRange(isize),
}

impl std::fmt::Debug for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RuntimeError::ValueStackUnderflow => write!(f, "value-stack underflow"),
            RuntimeError::IncorrectArity => write!(f, "incorrect arity"),
            RuntimeError::UndefinedGlobal(name) => write!(f, "undefined global {}", name.display()),
            RuntimeError::InvalidExitProcedure => write!(f, "invalid exit procedure"),
            RuntimeError::ClosedPort => write!(f, "access to closed port"),
            RuntimeError::WrongPortKind => write!(f, "wrong kind of port (input/output)"),
            RuntimeError::WriteError => write!(f, "write error"),
            RuntimeError::IndexOutOfRange(i) => write!(f, "index out of range: {}", i),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for RuntimeError {}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    WrongType,
    NotCallable(Scm),
    NoNumber(Scm),
    NoInt,
    NoU8,
    NoPositiveInt(Scm),
    NoPair(Scm),
    NoVector,
    NoBytevector(Scm),
    NoChar(Scm),
    NoSymbol,
    NoString(Scm),
    NoClosure,
    NoPort(Scm),
    NoRustObject(Scm),
    OutOfBounds,
    ValueOutOfRange(Scm),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for TypeError {}

#[derive(Debug)]
pub enum ErrorContext {
    None,
    Source(SourceLocation),
}

impl Error {
    pub fn at_expr(e: impl std::error::Error + 'static, expr: &TrackedSexpr) -> Self {
        Error::at_span(e, expr.source().clone())
    }

    pub fn at_span(e: impl std::error::Error + 'static, span: SourceLocation) -> Self {
        Error::new(e, ErrorContext::Source(span))
    }
}

impl From<std::convert::Infallible> for Error {
    fn from(_: std::convert::Infallible) -> Self {
        unreachable!()
    }
}

impl Error {
    pub fn from_parse_error_and_source(err: ParseError, src: Source) -> Error {
        assert_eq!(err.location.text, &*src.content);
        Error::new(
            err.kind,
            ErrorContext::Source(src.loc(err.location.start, err.location.end)),
        )
    }
}

macro_rules! impl_from_errors {
    ($($t:ty,)*) => {
        $(impl From<$t> for Error {
            fn from(e: $t) -> Self {
                Error::new(e, ErrorContext::None)
            }
        })*
    }
}

impl_from_errors!(
    CompileError,
    ObjectifyErrorKind,
    RuntimeError,
    TypeError,
    UnhandledException,
    std::io::Error,
    std::string::FromUtf8Error,
    std::str::Utf8Error,
);
