use crate::sexpr::TrackedSexpr;
use crate::source::Source;
use std::io::Read;
use std::path::PathBuf;
use std::rc::Rc;

use basic_parsers::{eof, tag, whitespace};
use combinators::{any, followed, map, peek};
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
    Tag(&'static str),
    Whitespace,
    Eof,
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

fn parse_delimiter(input: Span) -> ParseResult<()> {
    any((
        eof,
        map(tag("("), |_| ()),
        map(tag(")"), |_| ()),
        map(whitespace, |_| ()),
    ))(input)
    .map_err(|pe| ParseError {
        kind: ParseErrorKind::Context("Expected delimiter"),
        ..pe
    })
}

/*fn parse_sexpr<'a, E: ParseError<Span<'a>>>(src: Span<'a>) -> IResult<Span<'a>, SpannedSexpr<'a>, E> {
    not(tag(")"))(src)?;
    alt((
        parse_list,
        parse_abbreviation,
        parse_dot,
        parse_boolean,
        parse_number,
        parse_symbol,
    ))(src)
}

fn parse_list<'a, E: ParseError<Span<'a>>>(src: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let (rest, open) = tag("(")(src)?;
    let (rest, list) = context("list", separated_list(parse_whitespace, parse_sexpr))(rest)?;
    let (rest, close) = context(")", tag(")"))(rest)?;
    Ok((
        rest,
        SpannedSexpr {
            span: Span {
                extra: (),
                fragment: &src.fragment[..1 + close.offset - open.offset],
                line: open.line,
                offset: open.offset,
            },
            expr: Sexpr::List(list),
        },
    ))
}

fn parse_vector<'a, E: ParseError<Span<'a>>>(src: Span<'a>) -> IResult<Span, SpannedSexpr, E> {
    let (rest, open) = tag("#(")(src)?;
    let (rest, list) = separated_list(parse_whitespace, parse_sexpr)(rest)?;
    let (rest, close) = tag(")")(rest)?;
    Ok((
        rest,
        SpannedSexpr {
            span: Span {
                extra: (),
                fragment: &src.fragment[..1 + close.offset - open.offset],
                line: open.line,
                offset: open.offset,
            },
            expr: Sexpr::Vector(list),
        },
    ))
}

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
}

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
}*/

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

    pub fn eof<'a>(input: Span) -> ParseResult<()> {
        if input.is_empty() {
            Ok(((), input))
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
    use super::{ParseError, ParseErrorKind, ParseResult, Span};

    pub fn peek<'a, T>(
        parser: impl Fn(Span<'a>) -> ParseResult<'a, T>,
    ) -> impl Fn(Span<'a>) -> ParseResult<'a, T> {
        move |input: Span<'a>| -> ParseResult<'a, T> { parser(input).map(|(x, rest)| (x, input)) }
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

    pub fn any<'a, T>(
        parsers: impl CombinatorAny<'a, T>,
    ) -> impl Fn(Span<'a>) -> ParseResult<'a, T> {
        move |input: Span<'a>| -> ParseResult<'a, T> { parsers.accept_first(input) }
    }

    pub trait CombinatorAny<'a, T> {
        fn accept_first(&self, input: Span<'a>) -> ParseResult<'a, T>;
    }

    impl<'a, A, B, T> CombinatorAny<'a, T> for (A, B)
    where
        A: Fn(Span<'a>) -> ParseResult<'a, T>,
        B: Fn(Span<'a>) -> ParseResult<'a, T>,
    {
        fn accept_first(&self, input: Span<'a>) -> ParseResult<'a, T> {
            self.0(input).or_else(|_| self.1(input))
        }
    }

    impl<'a, A, B, C, T> CombinatorAny<'a, T> for (A, B, C)
    where
        A: Fn(Span<'a>) -> ParseResult<'a, T>,
        B: Fn(Span<'a>) -> ParseResult<'a, T>,
        C: Fn(Span<'a>) -> ParseResult<'a, T>,
    {
        fn accept_first(&self, input: Span<'a>) -> ParseResult<'a, T> {
            self.0(input)
                .or_else(|_| self.1(input))
                .or_else(|_| self.2(input))
        }
    }

    impl<'a, A, B, C, D, T> CombinatorAny<'a, T> for (A, B, C, D)
    where
        A: Fn(Span<'a>) -> ParseResult<'a, T>,
        B: Fn(Span<'a>) -> ParseResult<'a, T>,
        C: Fn(Span<'a>) -> ParseResult<'a, T>,
        D: Fn(Span<'a>) -> ParseResult<'a, T>,
    {
        fn accept_first(&self, input: Span<'a>) -> ParseResult<'a, T> {
            self.0(input)
                .or_else(|_| self.1(input))
                .or_else(|_| self.2(input))
                .or_else(|_| self.3(input))
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
        println!("{:#?}", parse_boolean(Span::new("#turnip")));
    }

    /*
    #[test]
    fn symbol_parsing() {
        compare!(
            Ok(Sexpr::Symbol("abc-def!")),
            parse_symbol::<(Span, ErrorKind)>(Span::new("abc-def!"))
        );
    }

    #[test]
    fn list_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("x"), Sexpr::Symbol("y"), Sexpr::Symbol("z")],
            parse_list::<(Span, ErrorKind)>(Span::new("(x y   z)")).unwrap().1.expr
        );
    }

    #[test]
    fn vector_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("x"), Sexpr::Symbol("y"), Sexpr::Symbol("z")],
            parse_vector::<(Span, ErrorKind)>(Span::new("#(x y   z)")).unwrap().1.expr
        );
    }

    #[test]
    fn number_parsing() {
        compare!(Ok(Sexpr::Integer(42)), parse_number::<(Span, ErrorKind)>(Span::new("42")));
        compare!(Ok(Sexpr::Integer(-24)), parse_number::<(Span, ErrorKind)>(Span::new("-24")));
        compare!(Ok(Sexpr::Float(3.1415)), parse_number::<(Span, ErrorKind)>(Span::new("3.1415")));
    }

    #[test]
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
