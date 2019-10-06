use lisp_core::lexpr;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    IoError(std::io::Error),
    ParseError(lexpr::parse::Error),
    UnexpectedType(lexpr::Value)
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IoError(e)
    }
}

impl From<lexpr::parse::Error> for Error {
    fn from(e: lexpr::parse::Error) -> Self {
        Error::ParseError(e)
    }
}
