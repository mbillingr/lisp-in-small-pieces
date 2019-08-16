use lexpr::parse::Error as LxError;
use rustyline::error::ReadlineError;

pub type LispResult<T> = std::result::Result<T, LispError>;

#[derive(Debug)]
pub struct LispError {
    /// Insipred by lexpr::parse::Error: This `Box` keeps the size of `LispError` small.
    pub kind: Box<ErrorKind>,
}

#[derive(Debug)]
pub enum ErrorKind {
    NoPair,
    NoRecord,
    OutOfBounds(usize, usize),

    Eof,

    ParseError(LxError),
    ReadLineError(ReadlineError),
}

impl<T> Into<LispResult<T>> for ErrorKind {
    fn into(self) -> LispResult<T> {
        Err(LispError {
            kind: Box::new(self),
        })
    }
}

impl<T: Into<ErrorKind>> From<T> for LispError {
    fn from(e: T) -> Self {
        LispError {
            kind: Box::new(e.into()),
        }
    }
}

impl From<LxError> for ErrorKind {
    fn from(lxe: LxError) -> Self {
        ErrorKind::ParseError(lxe)
    }
}

impl From<ReadlineError> for ErrorKind {
    fn from(rle: ReadlineError) -> Self {
        match rle {
            ReadlineError::Eof => ErrorKind::Eof,
            _ => ErrorKind::ReadLineError(rle),
        }
    }
}
