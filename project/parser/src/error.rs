use crate::span::Span;

pub type Result<'a, T> = std::result::Result<T, ParseError<'a>>;

pub type ParseResult<'a, T> = Result<'a, (T, Span<'a>)>;

#[derive(Debug)]
pub struct ParseError<'a> {
    pub kind: ParseErrorKind,
    pub location: Span<'a>,
    pub(crate) fatal: bool,
}

#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    Context(&'static str),
    Char(Option<char>),
    Tag(&'static str),
    Whitespace,
    Eof,
    Repeat(Box<ParseErrorKind>),
    Not,
    UnclosedSequence,
    InvalidToken,
}
