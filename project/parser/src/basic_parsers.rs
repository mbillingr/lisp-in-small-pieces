use crate::error::{ParseError, ParseErrorKind, ParseResult};
use crate::span::Span;

pub fn tag<'a>(tag: &'static str) -> impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>> {
    move |input: Span| -> ParseResult<Span> {
        if input.starts_with(tag) {
            Ok(input.split(tag.len()))
        } else {
            Err(ParseError {
                kind: ParseErrorKind::Tag(tag),
                location: input,
                fatal: false,
            })
        }
    }
}

pub fn char<'a>(tag: char) -> impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>> {
    move |input: Span| -> ParseResult<Span> {
        if input.starts_with(tag) {
            Ok(input.split(tag.len_utf8()))
        } else {
            Err(ParseError {
                kind: ParseErrorKind::Char(Some(tag)),
                location: input,
                fatal: false,
            })
        }
    }
}

pub fn char_that<'a>(
    predicate: impl Fn(char) -> bool,
) -> impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>> {
    move |input: Span| -> ParseResult<Span> {
        match input.chars().next() {
            Some(ch) if predicate(ch) => Ok(input.split(ch.len_utf8())),
            _ => Err(ParseError {
                kind: ParseErrorKind::Char(None),
                location: input,
                fatal: false,
            }),
        }
    }
}

pub fn any_char(input: Span) -> ParseResult<Span> {
    char_that(|_| true)(input)
}

pub fn eof<'a>(input: Span) -> ParseResult<Span> {
    if input.is_empty() {
        Ok((input, input))
    } else {
        Err(ParseError {
            kind: ParseErrorKind::Eof,
            location: input,
            fatal: false,
        })
    }
}

pub fn whitespace(input: Span) -> ParseResult<Span> {
    match input.char_indices().find(|(_, ch)| !ch.is_whitespace()) {
        None if input.is_empty() => Err(ParseError {
            kind: ParseErrorKind::Whitespace,
            location: input,
            fatal: false,
        }),
        None => Ok(input.split(input.len())),
        Some((0, _)) => Err(ParseError {
            kind: ParseErrorKind::Whitespace,
            location: input,
            fatal: false,
        }),
        Some((i, _)) => Ok(input.split(i)),
    }
}
