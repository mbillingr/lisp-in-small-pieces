use crate::error::{ParseError, ParseErrorKind, ParseResult};
use crate::span::{Span, Spanned};

pub fn peek<'a, T>(
    parser: impl Fn(Span<'a>) -> ParseResult<'a, T>,
) -> impl Fn(Span<'a>) -> ParseResult<'a, T> {
    move |input: Span<'a>| -> ParseResult<'a, T> { parser(input).map(|(x, _)| (x, input)) }
}

pub fn opt<'a, T>(
    parser: impl Fn(Span<'a>) -> ParseResult<'a, T>,
) -> impl Fn(Span<'a>) -> ParseResult<Option<T>> {
    move |input: Span<'a>| match parser(input) {
        Ok((out, rest)) => Ok((Some(out), rest)),
        Err(e) if !e.fatal => Ok((None, input)),
        Err(e) => Err(e),
    }
}

pub fn not<'a, T>(
    parser: impl Fn(Span<'a>) -> ParseResult<'a, T>,
) -> impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>> {
    move |input: Span<'a>| match parser(input) {
        Ok(_) => Err(ParseError {
            kind: ParseErrorKind::Not,
            location: input,
            fatal: false,
        }),
        Err(e) if !e.fatal => Ok(input.split(0)),
        Err(e) => Err(e),
    }
}

pub fn followed<'a, T, Z>(
    first: impl Fn(Span<'a>) -> ParseResult<'a, T>,
    by: impl Fn(Span<'a>) -> ParseResult<'a, Z>,
) -> impl Fn(Span<'a>) -> ParseResult<'a, T> {
    move |input: Span<'a>| -> ParseResult<'a, T> {
        let (a, rest) = first(input)?;
        let (_, rest) = by(rest)?;
        Ok((a, rest))
    }
}

pub fn map<'a, T, U>(
    parser: impl Fn(Span<'a>) -> ParseResult<'a, T>,
    func: impl Fn(T) -> U,
) -> impl Fn(Span<'a>) -> ParseResult<'a, U> {
    move |input: Span<'a>| -> ParseResult<'a, U> { parser(input).map(|(x, rest)| (func(x), rest)) }
}

pub fn repeat_0_or_more<'a>(
    parser: impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>>,
) -> impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>> {
    move |input: Span<'a>| -> ParseResult<'a, Span<'a>> {
        let mut rest = input;
        let mut matched = Span {
            end: input.start,
            ..input
        };
        while let Ok(x) = parser(rest) {
            matched.end = x.0.end;
            rest = x.1;
        }
        Ok((matched, rest))
    }
}

pub fn repeat_1_or_more<'a>(
    parser: impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>>,
) -> impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>> {
    move |input: Span<'a>| -> ParseResult<'a, Span<'a>> {
        let mut rest = input;
        let mut matched = Span {
            end: input.start,
            ..input
        };
        loop {
            match parser(rest) {
                Ok(x) => {
                    matched.end = x.0.end;
                    rest = x.1;
                }
                Err(mut e) => {
                    if matched.is_empty() {
                        e.kind = ParseErrorKind::Repeat(Box::new(e.kind));
                        return Err(e);
                    } else {
                        return Ok((matched, rest));
                    }
                }
            }
        }
    }
}

pub fn any<'a, T: Spanned<'a>>(
    parsers: impl ParserSequence<'a, T>,
) -> impl Fn(Span<'a>) -> ParseResult<'a, T> {
    move |input: Span<'a>| -> ParseResult<'a, T> { parsers.parse_or(input) }
}

pub fn all<'a, T: Spanned<'a>>(
    parsers: impl ParserSequence<'a, T>,
) -> impl Fn(Span<'a>) -> ParseResult<'a, T> {
    move |input: Span<'a>| -> ParseResult<'a, T> { parsers.parse_and(input) }
}

pub trait ParserSequence<'a, T: Spanned<'a>> {
    fn parse_or(&self, input: Span<'a>) -> ParseResult<'a, T>;
    fn parse_and(&self, input: Span<'a>) -> ParseResult<'a, T>;
}

impl<'a, A, B, T: Spanned<'a>> ParserSequence<'a, T> for (A, B)
where
    A: Fn(Span<'a>) -> ParseResult<'a, T>,
    B: Fn(Span<'a>) -> ParseResult<'a, T>,
{
    fn parse_or(&self, input: Span<'a>) -> ParseResult<'a, T> {
        match self.0(input) {
            Ok(x) => Ok(x),
            Err(e) if e.fatal => Err(e),
            Err(_) => self.1(input),
        }
    }

    fn parse_and(&self, input: Span<'a>) -> ParseResult<'a, T> {
        self.0(input)
            .and_then(|(_, rest)| self.1(rest))
            .map(|(mut out, rest)| {
                out.span_mut().start = input.start;
                (out, rest)
            })
    }
}

impl<'a, A, B, C, T: Spanned<'a>> ParserSequence<'a, T> for (A, B, C)
where
    A: Fn(Span<'a>) -> ParseResult<'a, T>,
    B: Fn(Span<'a>) -> ParseResult<'a, T>,
    C: Fn(Span<'a>) -> ParseResult<'a, T>,
{
    fn parse_or(&self, input: Span<'a>) -> ParseResult<'a, T> {
        any((&self.0, any((&self.1, &self.2))))(input)
    }

    fn parse_and(&self, input: Span<'a>) -> ParseResult<'a, T> {
        self.0(input)
            .and_then(|(_, rest)| self.1(rest))
            .and_then(|(_, rest)| self.2(rest))
            .map(|(mut out, rest)| {
                out.span_mut().start = input.start;
                (out, rest)
            })
    }
}

impl<'a, A, B, C, D, T: Spanned<'a>> ParserSequence<'a, T> for (A, B, C, D)
where
    A: Fn(Span<'a>) -> ParseResult<'a, T>,
    B: Fn(Span<'a>) -> ParseResult<'a, T>,
    C: Fn(Span<'a>) -> ParseResult<'a, T>,
    D: Fn(Span<'a>) -> ParseResult<'a, T>,
{
    fn parse_or(&self, input: Span<'a>) -> ParseResult<'a, T> {
        any((any((&self.0, &self.1)), any((&self.2, &self.3))))(input)
    }

    fn parse_and(&self, input: Span<'a>) -> ParseResult<'a, T> {
        self.0(input)
            .and_then(|(_, rest)| self.1(rest))
            .and_then(|(_, rest)| self.2(rest))
            .and_then(|(_, rest)| self.3(rest))
            .map(|(mut out, rest)| {
                out.span_mut().start = input.start;
                (out, rest)
            })
    }
}

impl<'a, A, B, C, D, E, T: Spanned<'a>> ParserSequence<'a, T> for (A, B, C, D, E)
where
    A: Fn(Span<'a>) -> ParseResult<'a, T>,
    B: Fn(Span<'a>) -> ParseResult<'a, T>,
    C: Fn(Span<'a>) -> ParseResult<'a, T>,
    D: Fn(Span<'a>) -> ParseResult<'a, T>,
    E: Fn(Span<'a>) -> ParseResult<'a, T>,
{
    fn parse_or(&self, input: Span<'a>) -> ParseResult<'a, T> {
        any((&self.0, &self.1, &self.2, any((&self.3, &self.4))))(input)
    }

    fn parse_and(&self, input: Span<'a>) -> ParseResult<'a, T> {
        self.0(input)
            .and_then(|(_, rest)| self.1(rest))
            .and_then(|(_, rest)| self.2(rest))
            .and_then(|(_, rest)| self.3(rest))
            .and_then(|(_, rest)| self.4(rest))
            .map(|(mut out, rest)| {
                out.span_mut().start = input.start;
                (out, rest)
            })
    }
}
