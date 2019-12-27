use crate::bytecode::Error as BytecodeError;
use crate::objectify::{ObjectifyError, ObjectifyErrorKind};
use crate::parsing::{ParseError, ParseErrorKind};
use crate::source::{Source, SourceLocation};

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub context: ErrorContext,
}

#[derive(Debug)]
pub enum ErrorKind {
    Parse(ParseErrorKind),
    Objectify(ObjectifyErrorKind),
    Runtime(BytecodeError),
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

impl From<BytecodeError> for Error {
    fn from(err: BytecodeError) -> Self {
        Error {
            kind: ErrorKind::Runtime(err),
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

impl PartialEq<BytecodeError> for Error {
    fn eq(&self, other: &BytecodeError) -> bool {
        match self.kind {
            ErrorKind::Runtime(ref e) => e == other,
            _ => false,
        }
    }
}

impl PartialEq<Error> for BytecodeError {
    fn eq(&self, other: &Error) -> bool {
        other.eq(self)
    }
}
