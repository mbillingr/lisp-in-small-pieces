use crate::objectify::{Error as ObjectifyError, ObjectifyErrorKind};
use crate::scm::{Error as ScmError, ErrorKind as ScmErrorKind, Scm};
use crate::sexpr::{Error as SexprError, ErrorKind as SexprErrorKind, TrackedSexpr};
use std::fmt::{Display, Formatter};
use sunny_common::{Source, SourceLocation, Symbol};
use sunny_parser::ParseErrorKind;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub context: ErrorContext,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub enum ErrorKind {
    Custom(Scm),
    Parse(ParseErrorKind),
    Objectify(ObjectifyErrorKind),
    Compile(CompileError),
    Runtime(RuntimeError),
    TypeError(TypeErrorKind<Scm>),
    IoError(std::io::Error),
    Utf8Error(std::str::Utf8Error),

    Unhandled(Scm),
}

impl ErrorKind {
    pub fn with_context(self, context: impl Into<ErrorContext>) -> Error {
        Error {
            kind: self,
            context: context.into(),
        }
    }
    pub fn without_context(self) -> Error {
        Error {
            kind: self,
            context: ErrorContext::None,
        }
    }
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
    ClosedPort,
    WrongPortKind,
    WriteError,
    IndexOutOfRange(isize),
}

impl std::error::Error for RuntimeError {}

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

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub struct TypeError<T> {
    pub kind: TypeErrorKind<T>,
    pub context: ErrorContext,
}

#[derive(Debug, PartialEq)]
pub enum TypeErrorKind<T> {
    NotCallable(T),
    NoNumber(T),
    NoInt(T),
    NoU8(T),
    NoPositiveInt(T),
    NoCell(T),
    NoPair(T),
    NoVector(T),
    NoBytevector(T),
    NoChar(T),
    NoSymbol(T),
    NoString(T),
    NoClosure(T),
    NoPort(T),
    NoRustObject(T),
    OutOfBounds(T, usize),
    ValueOutOfRange(T),
}

impl<T> TypeErrorKind<T> {
    pub fn with_context(self, context: impl Into<ErrorContext>) -> TypeError<T> {
        TypeError {
            kind: self,
            context: context.into(),
        }
    }
    pub fn without_context(self) -> TypeError<T> {
        TypeError {
            kind: self,
            context: ErrorContext::None,
        }
    }
}

impl<T> TypeErrorKind<T> {
    fn into<'a, S>(&'a self) -> TypeErrorKind<S>
    where
        S: From<&'a T>,
    {
        use TypeErrorKind::*;
        match self {
            NotCallable(x) => NotCallable(x.into()),
            NoNumber(x) => NoNumber(x.into()),
            NoInt(x) => NoInt(x.into()),
            NoU8(x) => NoU8(x.into()),
            NoPositiveInt(x) => NoPositiveInt(x.into()),
            NoCell(x) => NoCell(x.into()),
            NoPair(x) => NoPair(x.into()),
            NoVector(x) => NoVector(x.into()),
            NoBytevector(x) => NoBytevector(x.into()),
            NoChar(x) => NoChar(x.into()),
            NoSymbol(x) => NoSymbol(x.into()),
            NoString(x) => NoString(x.into()),
            NoClosure(x) => NoClosure(x.into()),
            NoPort(x) => NoPort(x.into()),
            NoRustObject(x) => NoRustObject(x.into()),
            OutOfBounds(x, idx) => OutOfBounds(x.into(), *idx),
            ValueOutOfRange(x) => ValueOutOfRange(x.into()),
        }
    }
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

impl From<TypeErrorKind<Scm>> for ErrorKind {
    fn from(err: TypeErrorKind<Scm>) -> Self {
        ErrorKind::TypeError(err)
    }
}

impl From<ObjectifyErrorKind> for ErrorKind {
    fn from(err: ObjectifyErrorKind) -> Self {
        ErrorKind::Objectify(err)
    }
}

impl From<ObjectifyError> for Error {
    fn from(err: ObjectifyError) -> Self {
        Error {
            kind: ErrorKind::Objectify(err.kind),
            context: err.context,
        }
    }
}

impl From<std::io::Error> for ErrorKind {
    fn from(err: std::io::Error) -> Self {
        ErrorKind::IoError(err)
    }
}

impl From<std::str::Utf8Error> for ErrorKind {
    fn from(err: std::str::Utf8Error) -> Self {
        ErrorKind::Utf8Error(err)
    }
}

impl From<std::string::FromUtf8Error> for ErrorKind {
    fn from(err: std::string::FromUtf8Error) -> Self {
        ErrorKind::Utf8Error(err.utf8_error())
    }
}

impl From<TypeError<Scm>> for Error {
    fn from(err: TypeError<Scm>) -> Self {
        Error {
            kind: ErrorKind::TypeError(err.kind),
            context: err.context,
        }
    }
}

impl From<SexprError> for Error {
    fn from(err: SexprError) -> Self {
        Error {
            kind: match err.kind {
                SexprErrorKind::ParseError(pek) => ErrorKind::Parse(pek),
                SexprErrorKind::TypeError(tek) => ErrorKind::TypeError((&tek).into()),
            },
            context: err.context,
        }
    }
}

impl From<ScmError> for Error {
    fn from(err: ScmError) -> Self {
        Error {
            kind: match err.kind {
                ScmErrorKind::TypeError(tek) => ErrorKind::TypeError((&tek).into()),
            },
            context: err.context,
        }
    }
}
/*
impl Error {
    pub fn from_parse_error_and_source(err: ParseError, src: Source) -> Error {
        assert_eq!(err.location.text, &*src.content);
        Error {
            kind: ErrorKind::Parse(err.kind),
            context: ErrorContext::Source(src.loc(err.location.start, err.location.end)),
        }
    }
}*/

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ErrorKind::Custom(e) => write!(f, "Error: {}", e.display()),
            ErrorKind::Parse(e) => write!(f, "Parse Error: {:?}", e),
            ErrorKind::Objectify(e) => write!(f, "Syntax Error: {:?}", e),
            ErrorKind::Runtime(e) => write!(f, "Runtime Error: {:?}", e),
            ErrorKind::Compile(e) => write!(f, "Compile Error: {:?}", e),
            ErrorKind::TypeError(e) => write!(f, "Type Error: {:?}", e),
            ErrorKind::IoError(e) => write!(f, "I/O Error: {:?}", e),
            ErrorKind::Utf8Error(e) => write!(f, "Utf8 Error: {:?}", e),
            ErrorKind::Unhandled(obj) => write!(f, "Unhandled Exception: {}", obj.write()),
        }
    }
}

impl From<SourceLocation> for ErrorContext {
    fn from(src: SourceLocation) -> Self {
        ErrorContext::Source(src)
    }
}

impl From<Source> for ErrorContext {
    fn from(src: Source) -> Self {
        ErrorContext::Source(src.into())
    }
}
