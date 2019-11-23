use crate::sexpr::TrackedSexpr;
use crate::source::Source;
use nom::combinator::all_consuming;
use nom::multi::{many0, separated_list};
use nom::number::complete::{double, recognize_float};
use nom::sequence::pair;
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::{alpha0, alphanumeric1, char, digit0, digit1, one_of},
    combinator::{map, not, opt, peek},
    multi::many1,
    sequence::{delimited, terminated},
    AsChar, IResult, InputTakeAtPosition,
};
use nom_locate::{position, LocatedSpan};
use std::io::Read;
use std::path::PathBuf;
use std::rc::Rc;

/*pub fn parse_string(src: impl Into<String>) -> TrackedSexpr {
    parse_source(src.into().into())
}

pub fn parse_file(file: impl Into<PathBuf>) -> TrackedSexpr {
    parse_source(Source::from_file(file).unwrap())
}*/

pub fn parse_source(src: Source) -> TrackedSexpr {
    unimplemented!()
}

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
pub struct SpannedSexpr<'a> {
    pub span: Span<'a>,
    pub expr: Sexpr<'a>,
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

pub fn parse(src: &str) -> SpannedSexpr {
    let (_, expr) = all_consuming(parse_sexpr)(Span::new(src)).unwrap();
    expr
}

fn parse_sexpr(src: Span) -> IResult<Span, SpannedSexpr> {
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

fn parse_list(src: Span) -> IResult<Span, SpannedSexpr> {
    let (rest, open) = tag("(")(src)?;
    let (rest, list) = separated_list(tag(" "), parse_sexpr)(rest)?;
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
            expr: Sexpr::List(list),
        },
    ))
}

fn parse_vector(src: Span) -> IResult<Span, SpannedSexpr> {
    let (rest, open) = tag("#(")(src)?;
    let (rest, list) = separated_list(tag(" "), parse_sexpr)(rest)?;
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

fn parse_abbreviation(rest: Span) -> IResult<Span, SpannedSexpr> {
    alt((
        parse_quote,
        parse_quasiquote,
        parse_unquote_splicing,
        parse_unquote,
    ))(rest)
}

fn parse_quote(src: Span) -> IResult<Span, SpannedSexpr> {
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

fn parse_quasiquote(src: Span) -> IResult<Span, SpannedSexpr> {
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

fn parse_unquote(src: Span) -> IResult<Span, SpannedSexpr> {
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

fn parse_unquote_splicing(src: Span) -> IResult<Span, SpannedSexpr> {
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

fn parse_dot(rest: Span) -> IResult<Span, SpannedSexpr> {
    map(terminated(tag("."), peek(parse_delimiter)), |s| {
        s.into_spanned(Sexpr::Dot)
    })(rest)
}

fn parse_boolean(rest: Span) -> IResult<Span, SpannedSexpr> {
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

fn parse_symbol(rest: Span) -> IResult<Span, SpannedSexpr> {
    let (rest, sym) = rest.split_at_position_complete(|item| !is_valid_symbol_char(item))?;
    Ok((rest, sym.into_spanned(Sexpr::Symbol(sym.fragment))))
}

fn parse_string(src: Span) -> IResult<Span, SpannedSexpr> {
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

fn parse_number(rest: Span) -> IResult<Span, SpannedSexpr> {
    alt((
        terminated(parse_integer, peek(parse_delimiter)),
        terminated(parse_float, peek(parse_delimiter)),
    ))(rest)
}

fn parse_integer(rest: Span) -> IResult<Span, SpannedSexpr> {
    let (rest, sign) = opt(tag("-"))(rest)?;
    let (rest, num) = digit1(rest)?;

    let mut i: i64 = num.fragment.parse().unwrap();
    if sign.is_some() {
        i = -i;
    }
    Ok((rest, num.into_spanned(Sexpr::Integer(i))))
}

fn parse_float(rest: Span) -> IResult<Span, SpannedSexpr> {
    let (rest, num) = recognize_float(rest)?;
    Ok((
        rest,
        num.into_spanned(Sexpr::Float(num.fragment.parse().unwrap())),
    ))
}

fn parse_delimiter(rest: Span) -> IResult<Span, ()> {
    alt((
        map(one_of(" ()"), |_| ()),
        not(take(1usize)), // EOF (can't take 1 more byte)
    ))(rest)
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
}

trait IntoSpannedSexpr<'a> {
    fn into_spanned(self, expr: Sexpr<'a>) -> SpannedSexpr<'a>;
}

impl<'a> IntoSpannedSexpr<'a> for Span<'a> {
    fn into_spanned(self, expr: Sexpr<'a>) -> SpannedSexpr<'a> {
        SpannedSexpr { span: self, expr }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! compare {
        ($expected:expr, $actual:expr) => {
            assert_eq!($actual.map(|s| s.1.expr), $expected)
        };
    }

    macro_rules! fail {
        ($actual:expr) => {
            assert!($actual.is_err())
        };
    }

    #[test]
    fn symbol_parsing() {
        compare!(
            Ok(Sexpr::Symbol("abc-def!")),
            parse_symbol(Span::new("abc-def!"))
        );
    }

    #[test]
    fn bool_parsing() {
        compare!(Ok(Sexpr::True), parse_boolean(Span::new("#true")));
        compare!(Ok(Sexpr::False), parse_boolean(Span::new("#false")));
        compare!(Ok(Sexpr::True), parse_boolean(Span::new("#t")));
        compare!(Ok(Sexpr::False), parse_boolean(Span::new("#f")));
        fail!(parse_boolean(Span::new("#turnip")));
    }

    #[test]
    fn list_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("x"), Sexpr::Symbol("y"), Sexpr::Symbol("z")],
            parse_list(Span::new("(x y z)")).unwrap().1.expr
        );
    }

    #[test]
    fn vector_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("x"), Sexpr::Symbol("y"), Sexpr::Symbol("z")],
            parse_vector(Span::new("#(x y z)")).unwrap().1.expr
        );
    }

    #[test]
    fn number_parsing() {
        compare!(Ok(Sexpr::Integer(42)), parse_number(Span::new("42")));
        compare!(Ok(Sexpr::Integer(-24)), parse_number(Span::new("-24")));
        compare!(Ok(Sexpr::Float(3.1415)), parse_number(Span::new("3.1415")));
    }

    #[test]
    fn string_parsing() {
        compare!(Ok(Sexpr::String("42")), parse_string(Span::new("\"42\"")));
    }

    #[test]
    fn quotation_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("quote"), Sexpr::Symbol("abc")],
            parse_quote(Span::new("'abc")).unwrap().1.expr
        );
    }

    #[test]
    fn quasiquotation_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("quasiquote"), Sexpr::Symbol("abc")],
            parse_quasiquote(Span::new("`abc")).unwrap().1.expr
        );
    }

    #[test]
    fn unquotation_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("unquote"), Sexpr::Symbol("abc")],
            parse_unquote(Span::new(",abc")).unwrap().1.expr
        );
    }

    #[test]
    fn splicing_unquotation_parsing() {
        assert_eq!(
            vec![Sexpr::Symbol("unquote-splicing"), Sexpr::Symbol("abc")],
            parse_unquote_splicing(Span::new(",@abc")).unwrap().1.expr
        );
    }

    #[test]
    fn sexpr_parsing() {
        let x = Span::new("(abc 123 (4.5 \"y\" . z))");
        parse_sexpr(x).unwrap();
    }
}
