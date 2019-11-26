use crate::error::ErrorKind::Objectify;
use crate::objectify::ObjectifyError;
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
    Objectify(ObjectifyError),
}

#[derive(Debug)]
pub enum ErrorContext {
    None,
    Source(SourceLocation),
}

impl From<ObjectifyError> for Error {
    fn from(err: ObjectifyError) -> Self {
        Error {
            kind: ErrorKind::Objectify(err),
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
        }
    }
}
