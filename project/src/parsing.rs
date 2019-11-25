use crate::sexpr::TrackedSexpr;
use crate::source::Source;
use std::io::Read;
use std::path::PathBuf;
use std::rc::Rc;

use crate::parsing::basic_parsers::char_that;
use crate::parsing::combinators::repeat_1_or_more;
use basic_parsers::{any_char, char, eof, tag, whitespace};
use combinators::{all, any, followed, map, not, opt, peek, repeat_0_or_more};
use std::io::SeekFrom::Current;

type Result<'a, T> = std::result::Result<T, ParseError<'a>>;

type ParseResult<'a, T> = Result<'a, (T, Span<'a>)>;

#[derive(Debug)]
pub struct ParseError<'a> {
    kind: ParseErrorKind,
    location: Span<'a>,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    Context(&'static str),
    Char(Option<char>),
    Tag(&'static str),
    Whitespace,
    Eof,
    Repeat(Box<ParseErrorKind>),
    Not,
}

pub trait Spanned<'a> {
    fn span(&self) -> &Span<'a>;
    fn span_mut(&mut self) -> &mut Span<'a>;
}

impl<'a> Spanned<'a> for Span<'a> {
    fn span(&self) -> &Span<'a> {
        self
    }
    fn span_mut(&mut self) -> &mut Span<'a> {
        self
    }
}

impl<'a> Spanned<'a> for SpannedSexpr<'a> {
    fn span(&self) -> &Span<'a> {
        &self.span
    }
    fn span_mut(&mut self) -> &mut Span<'a> {
        &mut self.span
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Span<'a> {
    text: &'a str,
    start: usize,
    end: usize,
}

impl<'a> std::ops::Deref for Span<'a> {
    type Target = str;
    fn deref(&self) -> &str {
        &self.text[self.start..self.end]
    }
}

impl<'a> Span<'a> {
    pub fn new(text: &'a str) -> Self {
        Span {
            text,
            start: 0,
            end: text.len(),
        }
    }

    pub fn range(from: Span<'a>, to: Span<'a>) -> Self {
        assert_eq!(from.text, to.text);
        Span {
            text: from.text,
            start: from.start,
            end: to.end,
        }
    }

    pub fn empty(&self) -> bool {
        self.end == self.start
    }

    pub fn split(&self, n: usize) -> (Self, Self) {
        let first = Span {
            text: self.text,
            start: self.start,
            end: self.start + n,
        };
        let second = Span {
            text: self.text,
            start: self.start + n,
            end: self.end,
        };
        (first, second)
    }
}

#[derive(Debug)]
pub struct SpannedSexpr<'a> {
    pub span: Span<'a>,
    pub expr: Sexpr<'a>,
}

impl<'a> PartialEq for SpannedSexpr<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.expr.eq(&other.expr)
    }
}

#[derive(Debug, PartialEq)]
pub enum Sexpr<'a> {
    True,
    False,
    Symbol(&'a str),
    String(&'a str),
    Integer(i64),
    Float(f64),
    List(Vec<SpannedSexpr<'a>>),
    Vector(Vec<SpannedSexpr<'a>>),

    Dot,
}

pub fn parse(src: &str) -> Result<SpannedSexpr> {
    unimplemented!()
    //let (rest, expr) = all_consuming(parse_sexpr)(Span::new(src))?;
    //Ok((rest, expr))
}

fn parse_sexpr(src: Span) -> ParseResult<SpannedSexpr> {
    not(char(')'))(src)?;
    any((
        //parse_abbreviation,
        //parse_dot,
        parse_boolean,
        parse_symbol,
        parse_number,
        parse_list,
        parse_vector,
    ))(src)
}

fn parse_list(input: Span) -> ParseResult<SpannedSexpr> {
    let (open, rest) = char('(')(input)?;
    let (list, rest) = parse_sequence(rest)?;
    let (close, rest) = char(')')(rest)?;

    let final_span = Span::range(open, close);
    Ok((final_span.into_spanned(Sexpr::List(list)), rest))
}

fn parse_vector(input: Span) -> ParseResult<SpannedSexpr> {
    let (open, rest) = tag("#(")(input)?;
    let (list, rest) = parse_sequence(rest)?;
    let (close, rest) = char(')')(rest)?;

    let final_span = Span::range(open, close);
    Ok((final_span.into_spanned(Sexpr::Vector(list)), rest))
}

fn parse_sequence(input: Span) -> ParseResult<Vec<SpannedSexpr>> {
    let (_, mut rest) = opt(whitespace)(input);

    let mut seq = vec![];

    while let Ok((item, r)) = parse_sexpr(rest) {
        seq.push(item);
        let (_, r) = opt(whitespace)(r);
        rest = r;
    }

    Ok((seq, rest))
}

fn parse_boolean(input: Span) -> ParseResult<SpannedSexpr> {
    followed(any((parse_true, parse_false)), peek(parse_delimiter))(input).map_err(|pe| {
        ParseError {
            kind: ParseErrorKind::Context("Expected boolean: #t, #f, #true, or #false"),
            location: input,
        }
    })
}

fn parse_true(input: Span) -> ParseResult<SpannedSexpr> {
    any((tag("#true"), tag("#t")))(input).map(|(span, rest)| (span.into_spanned(Sexpr::True), rest))
}

fn parse_false(input: Span) -> ParseResult<SpannedSexpr> {
    any((tag("#false"), tag("#f")))(input)
        .map(|(span, rest)| (span.into_spanned(Sexpr::False), rest))
}

fn parse_number(input: Span) -> ParseResult<SpannedSexpr> {
    let (num, rest) = repeat_1_or_more(all((not(parse_delimiter), any_char)))(input)?;

    if let Ok(i) = num.parse() {
        return Ok((num.into_spanned(Sexpr::Integer(i)), rest));
    }

    if let Ok(f) = num.parse() {
        return Ok((num.into_spanned(Sexpr::Float(f)), rest));
    }

    Err(ParseError {
        kind: ParseErrorKind::Context("Expected number"),
        location: input,
    })
}

fn parse_symbol(input: Span) -> ParseResult<SpannedSexpr> {
    all((
        parse_symbol_initial,
        repeat_0_or_more(parse_symbol_subsequent),
    ))(input)
    .map(|(span, rest)| {
        (
            span.into_spanned(Sexpr::Symbol(&span.text[span.start..span.end])),
            rest,
        )
    })
}

fn parse_symbol_initial(input: Span) -> ParseResult<Span> {
    any((
        char_that(char::is_alphabetic),
        any((char('!'), char('$'), char('%'), char('&'), char('*'))),
        any((char('/'), char(':'), char('<'), char('='), char('>'))),
        any((char('?'), char('^'), char('_'), char('~'))),
    ))(input)
}

fn parse_symbol_subsequent(input: Span) -> ParseResult<Span> {
    any((
        parse_symbol_initial,
        char_that(|ch| ch.is_ascii_digit()),
        any((char('+'), char('-'), char('.'), char('@'))),
    ))(input)
}

fn delimited_tag<'a>(name: &'static str) -> impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>> {
    followed(tag(name), peek(parse_delimiter))
}

fn parse_delimiter(input: Span) -> ParseResult<Span> {
    any((eof, tag("("), tag(")"), whitespace))(input).map_err(|pe| ParseError {
        kind: ParseErrorKind::Context("Expected delimiter"),
        ..pe
    })
}

/*
fn parse_abbreviation<'a, E: ParseError<Span<'a>>>(rest: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    alt((
        parse_quote,
        parse_quasiquote,
        parse_unquote_splicing,
        parse_unquote,
    ))(rest)
}

fn parse_quote<'a, E: ParseError<Span<'a>>>(src: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let (rest, prefix) = tag("'")(src)?;
    let (rest, expr) = parse_sexpr(rest)?;
    Ok((
        rest,
        SpannedSexpr {
            span: Span {
                extra: (),
                fragment: &src.fragment[..1 + expr.span.fragment.len()],
                line: prefix.line,
                offset: prefix.offset,
            },
            expr: Sexpr::List(vec![prefix.into_spanned(Sexpr::Symbol("quote")), expr]),
        },
    ))
}

fn parse_quasiquote<'a, E: ParseError<Span<'a>>>(src: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let (rest, prefix) = tag("`")(src)?;
    let (rest, expr) = parse_sexpr(rest)?;
    Ok((
        rest,
        SpannedSexpr {
            span: Span {
                extra: (),
                fragment: &src.fragment[..1 + expr.span.fragment.len()],
                line: prefix.line,
                offset: prefix.offset,
            },
            expr: Sexpr::List(vec![prefix.into_spanned(Sexpr::Symbol("quasiquote")), expr]),
        },
    ))
}

fn parse_unquote<'a, E: ParseError<Span<'a>>>(src: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let (rest, prefix) = tag(",")(src)?;
    let (rest, expr) = parse_sexpr(rest)?;
    Ok((
        rest,
        SpannedSexpr {
            span: Span {
                extra: (),
                fragment: &src.fragment[..1 + expr.span.fragment.len()],
                line: prefix.line,
                offset: prefix.offset,
            },
            expr: Sexpr::List(vec![prefix.into_spanned(Sexpr::Symbol("unquote")), expr]),
        },
    ))
}

fn parse_unquote_splicing<'a, E: ParseError<Span<'a>>>(src: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let (rest, prefix) = tag(",@")(src)?;
    let (rest, expr) = parse_sexpr(rest)?;
    Ok((
        rest,
        SpannedSexpr {
            span: Span {
                extra: (),
                fragment: &src.fragment[..1 + expr.span.fragment.len()],
                line: prefix.line,
                offset: prefix.offset,
            },
            expr: Sexpr::List(vec![
                prefix.into_spanned(Sexpr::Symbol("unquote-splicing")),
                expr,
            ]),
        },
    ))
}

fn parse_dot<'a, E: ParseError<Span<'a>>>(rest: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    map(terminated(tag("."), peek(parse_delimiter)), |s| {
        s.into_spanned(Sexpr::Dot)
    })(rest)
}

fn parse_boolean<'a, E: ParseError<Span<'a>>>(rest: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let item = alt((
        map(alt((tag("#true"), tag("#t"))), |span: Span| {
            span.into_spanned(Sexpr::True)
        }),
        map((alt((tag("#false"), tag("#f")))), |span: Span| {
            span.into_spanned(Sexpr::False)
        }),
    ));
    terminated(item, peek(parse_delimiter))(rest)
}

fn parse_symbol<'a, E: ParseError<Span<'a>>>(rest: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let (rest, sym) = rest.split_at_position_complete(|item| !is_valid_symbol_char(item))?;
    Ok((rest, sym.into_spanned(Sexpr::Symbol(sym.fragment))))
}

fn parse_string<'a, E: ParseError<Span<'a>>>(src: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let (rest, open) = tag("\"")(src)?;
    let (rest, string) = rest.split_at_position_complete(|item| item == '"')?;
    let (rest, close) = tag("\"")(rest)?;

    Ok((
        rest,
        SpannedSexpr {
            span: Span {
                extra: (),
                fragment: &src.fragment[..1 + close.offset - open.offset],
                line: open.line,
                offset: open.offset,
            },
            expr: Sexpr::String(string.fragment),
        },
    ))
}

fn parse_number<'a, E: ParseError<Span<'a>>>(rest: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    alt((
        terminated(parse_integer, peek(parse_delimiter)),
        terminated(parse_float, peek(parse_delimiter)),
    ))(rest)
}

fn parse_integer<'a, E: ParseError<Span<'a>>>(rest: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let (rest, sign) = opt(tag("-"))(rest)?;
    let (rest, num) = digit1(rest)?;

    let mut i: i64 = num.fragment.parse().unwrap();
    if sign.is_some() {
        i = -i;
    }
    Ok((rest, num.into_spanned(Sexpr::Integer(i))))
}

fn parse_float<'a, E: ParseError<Span<'a>>>(rest: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let (rest, num) = recognize_float(rest)?;
    Ok((
        rest,
        num.into_spanned(Sexpr::Float(num.fragment.parse().unwrap())),
    ))
}

fn parse_delimiter<'a, E: ParseError<Span<'a>>>(rest: Span<'a>) -> IResult<Span, (), E> {
    alt((
        map(one_of(" ()"), |_| ()),
        not(take(1usize)), // EOF (can't take 1 more byte)
    ))(rest)
}

fn parse_whitespace<'a, E: ParseError<Span<'a>>>(i: Span<'a>) -> IResult<Span, Span, E> {
    space1(i)
}

fn is_valid_symbol_char(item: char) -> bool {
    match item {
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '_' | '~' => {
            true
        }
        '+' | '-' => true,
        '.' => true,
        ch => ch.is_ascii_alphanumeric(),
    }
}*/

impl<'a> PartialEq<Sexpr<'a>> for SpannedSexpr<'a> {
    fn eq(&self, rhs: &Sexpr<'a>) -> bool {
        self.expr.eq(rhs)
    }
}

impl<'a> PartialEq<Vec<Sexpr<'a>>> for Sexpr<'a> {
    fn eq(&self, rhs: &Vec<Sexpr<'a>>) -> bool {
        match self {
            Sexpr::List(x) => x == rhs,
            Sexpr::Vector(x) => x == rhs,
            _ => false,
        }
    }
}

impl<'a> PartialEq<Sexpr<'a>> for Vec<Sexpr<'a>> {
    fn eq(&self, rhs: &Sexpr<'a>) -> bool {
        rhs.eq(self)
    }
}

trait IntoSpannedSexpr<'a> {
    fn into_spanned(self, expr: Sexpr<'a>) -> SpannedSexpr<'a>;
}

impl<'a> IntoSpannedSexpr<'a> for Span<'a> {
    fn into_spanned(self, expr: Sexpr<'a>) -> SpannedSexpr<'a> {
        SpannedSexpr { span: self, expr }
    }
}

mod basic_parsers {
    use super::{ParseError, ParseErrorKind, ParseResult, Span};

    pub fn tag<'a>(tag: &'static str) -> impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>> {
        move |input: Span| -> ParseResult<Span> {
            if input.starts_with(tag) {
                Ok(input.split(tag.len()))
            } else {
                Err(ParseError {
                    kind: ParseErrorKind::Tag(tag),
                    location: input,
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
            })
        }
    }

    pub fn whitespace(input: Span) -> ParseResult<Span> {
        match input.char_indices().find(|(_, ch)| !ch.is_whitespace()) {
            None if input.is_empty() => Err(ParseError {
                kind: ParseErrorKind::Whitespace,
                location: input,
            }),
            None => Ok(input.split(input.len())),
            Some((0, ch)) => Err(ParseError {
                kind: ParseErrorKind::Whitespace,
                location: input,
            }),
            Some((i, ch)) => Ok(input.split(i)),
        }
    }
}

mod combinators {
    use super::{ParseError, ParseErrorKind, ParseResult, Span, Spanned};
    use std::io::ErrorKind;

    pub fn peek<'a, T>(
        parser: impl Fn(Span<'a>) -> ParseResult<'a, T>,
    ) -> impl Fn(Span<'a>) -> ParseResult<'a, T> {
        move |input: Span<'a>| -> ParseResult<'a, T> { parser(input).map(|(x, rest)| (x, input)) }
    }

    pub fn opt<'a, T>(
        parser: impl Fn(Span<'a>) -> ParseResult<'a, T>,
    ) -> impl Fn(Span<'a>) -> (Option<T>, Span<'a>) {
        move |input: Span<'a>| {
            parser(input)
                .map(|(out, rest)| (Some(out), rest))
                .unwrap_or((None, input))
        }
    }

    pub fn not<'a, T>(
        parser: impl Fn(Span<'a>) -> ParseResult<'a, T>,
    ) -> impl Fn(Span<'a>) -> ParseResult<'a, Span<'a>> {
        move |input: Span<'a>| match parser(input) {
            Ok(_) => Err(ParseError {
                kind: ParseErrorKind::Not,
                location: input,
            }),
            Err(_) => Ok(input.split(0)),
        }
    }

    pub fn followed<'a, T, Z>(
        first: impl Fn(Span<'a>) -> ParseResult<'a, T>,
        second: impl Fn(Span<'a>) -> ParseResult<'a, Z>,
    ) -> impl Fn(Span<'a>) -> ParseResult<'a, T> {
        move |input: Span<'a>| -> ParseResult<'a, T> {
            let (a, rest) = first(input)?;
            let (_, rest) = second(rest)?;
            Ok((a, rest))
        }
    }

    pub fn map<'a, T, U>(
        parser: impl Fn(Span<'a>) -> ParseResult<'a, T>,
        func: impl Fn(T) -> U,
    ) -> impl Fn(Span<'a>) -> ParseResult<'a, U> {
        move |input: Span<'a>| -> ParseResult<'a, U> {
            parser(input).map(|(x, rest)| (func(x), rest))
        }
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
            self.0(input).or_else(|_| self.1(input))
        }

        fn parse_and(&self, input: Span<'a>) -> ParseResult<'a, T> {
            self.0(input)
                .and_then(|_| self.1(input))
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
            self.0(input)
                .or_else(|_| self.1(input))
                .or_else(|_| self.2(input))
        }

        fn parse_and(&self, input: Span<'a>) -> ParseResult<'a, T> {
            self.0(input)
                .and_then(|_| self.1(input))
                .and_then(|_| self.2(input))
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
            self.0(input)
                .or_else(|_| self.1(input))
                .or_else(|_| self.2(input))
                .or_else(|_| self.3(input))
        }

        fn parse_and(&self, input: Span<'a>) -> ParseResult<'a, T> {
            self.0(input)
                .and_then(|_| self.1(input))
                .and_then(|_| self.2(input))
                .and_then(|_| self.3(input))
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
            self.0(input)
                .or_else(|_| self.1(input))
                .or_else(|_| self.2(input))
                .or_else(|_| self.3(input))
                .or_else(|_| self.4(input))
        }

        fn parse_and(&self, input: Span<'a>) -> ParseResult<'a, T> {
            self.0(input)
                .and_then(|_| self.1(input))
                .and_then(|_| self.2(input))
                .and_then(|_| self.3(input))
                .and_then(|_| self.4(input))
                .map(|(mut out, rest)| {
                    out.span_mut().start = input.start;
                    (out, rest)
                })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! compare {
        ($expected:expr, $actual:expr) => {
            match $actual {
                Ok((ex, rest)) => {
                    assert_eq!(ex.expr, $expected);
                    assert!(rest.empty())
                }
                Err(e) => panic!("{:#?}", e),
            }
        };

        ($expected:expr, _, $actual:expr) => {
            match $actual {
                Ok((ex, rest)) => {
                    assert_eq!(ex.expr, $expected);
                }
                Err(e) => panic!("{:#?}", e),
            }
        };
    }

    macro_rules! fail {
        ($actual:expr) => {
            assert!($actual.is_err())
        };
    }

    #[test]
    fn bool_parsing() {
        compare!(Sexpr::True, parse_boolean(Span::new("#true")));
        compare!(Sexpr::False, parse_boolean(Span::new("#false")));
        compare!(Sexpr::True, parse_boolean(Span::new("#t")));
        compare!(Sexpr::False, parse_boolean(Span::new("#f")));
        fail!(parse_boolean(Span::new("#turnip")));
    }

    #[test]
    fn symbol_parsing() {
        compare!(
            Sexpr::Symbol("abc-def!"),
            parse_symbol(Span::new("abc-def!"))
        );
        compare!(Sexpr::Symbol("x"), _, parse_symbol(Span::new("x(y)")));
        compare!(Sexpr::Symbol("x"), _, parse_symbol(Span::new("x y")));
    }

    #[test]
    fn list_parsing() {
        compare!(
            vec![Sexpr::Symbol("x"), Sexpr::Symbol("y"), Sexpr::Symbol("z")],
            parse_list(Span::new("(x y z)"))
        );

        compare!(
            vec![Sexpr::Symbol("x"), Sexpr::Symbol("y"), Sexpr::Symbol("z")],
            parse_list(Span::new("(   x   y   z    )"))
        );
    }

    #[test]
    fn nested_list_parsing() {
        let result = parse_list(Span::new("(x ((y) z))")).unwrap().0.expr;
        if let Sexpr::List(items) = result {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0].expr, Sexpr::Symbol("x"));
            if let Sexpr::List(items) = &items[1].expr {
                assert_eq!(items.len(), 2);
                if let Sexpr::List(items) = &items[0].expr {
                    assert_eq!(items.len(), 1);
                    assert_eq!(items[0].expr, Sexpr::Symbol("y"));
                }else {
                    panic!("inner-most list not parsed correctly")
                }
                assert_eq!(items[1].expr, Sexpr::Symbol("z"));
            } else {
                panic!("inner list not parsed correctly")
            }
        } else {
            panic!("outer list not parsed correctly")
        }
    }

    #[test]
    fn vector_parsing() {
        compare!(
            vec![Sexpr::Symbol("x"), Sexpr::Symbol("y"), Sexpr::Symbol("z")],
            parse_vector(Span::new("#(  x  y z   )"))
        );
    }

    #[test]
    fn number_parsing() {
        compare!(Sexpr::Integer(42), parse_number(Span::new("42")));
        compare!(Sexpr::Integer(-24), parse_number(Span::new("-24")));
        compare!(Sexpr::Float(3.1415), parse_number(Span::new("3.1415")));
        fail!(parse_number(Span::new("1x2y3")))
    }

    /*#[test]
    fn string_parsing() {
        compare!(Ok(Sexpr::String("42")), parse_string::<(Span, ErrorKind)>(Span::new("\"42\"")));
    }

    #[test]
    fn quotation_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("quote"), Sexpr::Symbol("abc")],
            parse_quote::<(Span, ErrorKind)>(Span::new("'abc")).unwrap().1.expr
        );
    }

    #[test]
    fn quasiquotation_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("quasiquote"), Sexpr::Symbol("abc")],
            parse_quasiquote::<(Span, ErrorKind)>(Span::new("`abc")).unwrap().1.expr
        );
    }

    #[test]
    fn unquotation_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("unquote"), Sexpr::Symbol("abc")],
            parse_unquote::<(Span, ErrorKind)>(Span::new(",abc")).unwrap().1.expr
        );
    }

    #[test]
    fn splicing_unquotation_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("unquote-splicing"), Sexpr::Symbol("abc")],
            parse_unquote_splicing::<(Span, ErrorKind)>(Span::new(",@abc")).unwrap().1.expr
        );
    }

    #[test]
    fn sexpr_parsing() {
        let x = Span::new("(abc 123 (4.5 \"y\" . z))");
        parse_sexpr::<(Span, ErrorKind)>(x).unwrap();
    }*/
}
