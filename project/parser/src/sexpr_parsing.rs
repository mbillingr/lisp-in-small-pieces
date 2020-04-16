use crate::basic_parsers::char_that;
use crate::basic_parsers::{any_char, char, eof, tag, whitespace};
use crate::combinators::repeat_1_or_more;
use crate::combinators::{all, any, followed, map, not, opt, peek, repeat_0_or_more};
use crate::error::{ParseError, ParseErrorKind, ParseResult, Result};
use crate::sexpr::{IntoSpannedSexpr, Sexpr, SpannedSexpr};
use crate::span::Span;

pub fn parse(src: &str) -> Result<Vec<SpannedSexpr>> {
    let mut exprs = vec![];
    let mut src = Span::new(src);
    while !src.is_empty() {
        let (expr, rest) = parse_sexpr(src)?;
        src = rest;
        exprs.push(expr);
    }
    Ok(exprs)
}

pub fn parse_sexpr(src: Span) -> ParseResult<SpannedSexpr> {
    let (_, src) = opt(parse_intertoken_space)(src)?;
    let (expr, rest) = any((
        any((parse_abbreviation, parse_dot, parse_boolean, parse_number)),
        any((parse_character, parse_symbol)),
        any((parse_list, parse_vector, parse_string)),
        parse_invalid,
    ))(src)?;
    let (_, rest) = opt(parse_intertoken_space)(rest)?;
    Ok((expr, rest))
}

fn parse_intertoken_space(src: Span) -> ParseResult<Span> {
    repeat_1_or_more(any((whitespace, parse_comment)))(src)
}

fn parse_invalid<T>(input: Span) -> ParseResult<T> {
    Err(ParseError {
        kind: ParseErrorKind::InvalidToken,
        location: input,
        fatal: true,
    })
}

fn parse_comment(src: Span) -> ParseResult<Span> {
    all((
        char(';'),
        repeat_0_or_more(all((not(parse_newline), any_char))),
    ))(src)
}

fn parse_newline(src: Span) -> ParseResult<Span> {
    (char('\n'))(src)
}

fn parse_dot(input: Span) -> ParseResult<SpannedSexpr> {
    followed(
        map(char('.'), |span| span.into_spanned(Sexpr::Dot)),
        peek(parse_delimiter),
    )(input)
}

fn parse_list(input: Span) -> ParseResult<SpannedSexpr> {
    let (open, rest) = char('(')(input)?;
    let (list, rest) = parse_sequence(rest)?;
    let (close, rest) = char(')')(rest).map_err(|_| ParseError {
        kind: ParseErrorKind::UnclosedSequence,
        location: input,
        fatal: true,
    })?;

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
    let (_, mut rest) = opt(whitespace)(input).unwrap();

    let mut seq = vec![];

    loop {
        if let Ok(_) = char(')')(rest) {
            return Ok((seq, rest));
        }

        if let Ok(_) = eof(rest) {
            return Ok((seq, rest));
        }

        let (item, r) = parse_sexpr(rest)?;
        seq.push(item);
        let (_, r) = opt(whitespace)(r).unwrap();
        rest = r;
    }
}

fn parse_boolean(input: Span) -> ParseResult<SpannedSexpr> {
    followed(any((parse_true, parse_false)), peek(parse_delimiter))(input).map_err(|_| ParseError {
        kind: ParseErrorKind::Context("Expected boolean: #t, #f, #true, or #false"),
        location: input,
        fatal: false,
    })
}

fn parse_true(input: Span) -> ParseResult<SpannedSexpr> {
    map(any((tag("#true"), tag("#t"))), |span| {
        span.into_spanned(Sexpr::True)
    })(input)
}

fn parse_false(input: Span) -> ParseResult<SpannedSexpr> {
    map(any((tag("#false"), tag("#f"))), |span| {
        span.into_spanned(Sexpr::False)
    })(input)
}

fn parse_number(input: Span) -> ParseResult<SpannedSexpr> {
    any((parse_infnan, parse_normal_number))(input)
}

fn parse_infnan(input: Span) -> ParseResult<SpannedSexpr> {
    any((
        map(tag("+inf.0"), |span| {
            span.into_spanned(Sexpr::Float(std::f64::INFINITY))
        }),
        map(tag("-inf.0"), |span| {
            span.into_spanned(Sexpr::Float(std::f64::NEG_INFINITY))
        }),
        map(tag("+nan.0"), |span| {
            span.into_spanned(Sexpr::Float(std::f64::NAN))
        }),
        map(tag("-nan.0"), |span| {
            span.into_spanned(Sexpr::Float(std::f64::NAN))
        }),
    ))(input)
}

fn parse_normal_number(input: Span) -> ParseResult<SpannedSexpr> {
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
        fatal: false,
    })
}

fn parse_character(input: Span) -> ParseResult<SpannedSexpr> {
    any((
        any((
            parse_literal_character,
            parse_named_character(r"#\alarm", '\u{0007}'),
            parse_named_character(r"#\backspace", '\u{0008}'),
            parse_named_character(r"#\delete", '\u{007f}'),
        )),
        any((
            parse_named_character(r"#\escape", '\u{001b}'),
            parse_named_character(r"#\newline", '\u{000a}'),
            parse_named_character(r"#\null", '\u{0000}'),
            parse_named_character(r"#\return", '\u{000d}'),
        )),
        any((
            parse_named_character(r"#\space", ' '),
            parse_named_character(r"#\tab", '\u{0009}'),
        )),
    ))(input)
}

fn parse_literal_character(input: Span) -> ParseResult<SpannedSexpr> {
    let (t, rest) = tag(r"#\")(input)?;
    let (first_ch, rest) = any_char(rest)?;
    peek(parse_delimiter)(rest)?;
    //let (rest_ch, _) = repeat_0_or_more(all((not(parse_delimiter), any_char)))(rest)?;

    let span = Span::range(t, first_ch);
    let ch = first_ch.chars().next().unwrap();
    Ok(((span.into_spanned(Sexpr::Char(ch))), rest))
}

fn parse_named_character<'a>(
    name: &'static str,
    ch: char,
) -> impl Fn(Span<'a>) -> ParseResult<SpannedSexpr> {
    map(tag(name), move |span| span.into_spanned(Sexpr::Char(ch)))
}

fn parse_symbol(input: Span) -> ParseResult<SpannedSexpr> {
    map(
        any((
            all((
                parse_symbol_initial,
                repeat_0_or_more(parse_symbol_subsequent),
            )),
            //all((char('|'), repeat_0_or_more(parse_symbol_element), char('|'))),  //not yet implemented
            parse_peculiar_identifier,
        )),
        |span| span.into_spanned(Sexpr::Symbol(&span.text[span.start..span.end])),
    )(input)
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
        any((parse_explicit_sign, char('.'), char('@'))),
    ))(input)
}

fn parse_peculiar_identifier(input: Span) -> ParseResult<Span> {
    any((
        all((
            parse_explicit_sign,
            parse_sign_subsequent,
            repeat_0_or_more(parse_symbol_subsequent),
        )),
        all((
            parse_explicit_sign,
            char('.'),
            parse_dot_subsequent,
            repeat_0_or_more(parse_symbol_subsequent),
        )),
        all((
            char('.'),
            parse_dot_subsequent,
            repeat_0_or_more(parse_symbol_subsequent),
        )),
        parse_explicit_sign,
    ))(input)
}

fn parse_dot_subsequent(input: Span) -> ParseResult<Span> {
    any((parse_sign_subsequent, char('.')))(input)
}

fn parse_sign_subsequent(input: Span) -> ParseResult<Span> {
    any((parse_symbol_initial, parse_explicit_sign, char('@')))(input)
}

fn parse_explicit_sign(input: Span) -> ParseResult<Span> {
    any((char('+'), char('-')))(input)
}

fn parse_string(input: Span) -> ParseResult<SpannedSexpr> {
    let (open, rest) = char('"')(input)?;
    let (span, rest) = repeat_0_or_more(all((not(char('"')), any_char)))(rest)?;
    let (close, rest) = char('"')(rest)?;

    let final_span = Span::range(open, close);
    Ok((
        final_span.into_spanned(Sexpr::String(&span.text[span.start..span.end])),
        rest,
    ))
}

fn parse_abbreviation(input: Span) -> ParseResult<SpannedSexpr> {
    any((
        map(tag("'()"), |span| span.into_spanned(Sexpr::Nil)),
        expand("'", "quote"),
        expand("`", "quasiquote"),
        expand(",@", "unquote-splicing"),
        expand(",", "unquote"),
    ))(input)
}

fn expand<'a>(
    prefix: &'static str,
    symbol: &'static str,
) -> impl Fn(Span<'a>) -> ParseResult<'a, SpannedSexpr<'a>> {
    move |input| {
        let (prefix, rest) = tag(prefix)(input)?;
        let (expr, rest) = parse_sexpr(rest)?;

        let final_span = Span::range(prefix, expr.span);
        Ok((
            final_span.into_spanned(Sexpr::List(vec![
                prefix.into_spanned(Sexpr::Symbol(symbol)),
                expr,
            ])),
            rest,
        ))
    }
}

fn parse_delimiter(input: Span) -> ParseResult<Span> {
    any((eof, tag("("), tag(")"), whitespace))(input).map_err(|pe| ParseError {
        kind: ParseErrorKind::Context("Expected delimiter"),
        ..pe
    })
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
                Ok((ex, _)) => {
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
        compare!(Sexpr::Symbol("..."), _, parse_symbol(Span::new("...")));
        compare!(Sexpr::Symbol("-x"), _, parse_symbol(Span::new("-x")));
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

        compare!(
            vec![
                Sexpr::Symbol("x"),
                Sexpr::Symbol("y"),
                Sexpr::Dot,
                Sexpr::Symbol("z")
            ],
            parse_list(Span::new("(x y . z)"))
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
                } else {
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
        compare!(
            Sexpr::Float(std::f64::INFINITY),
            parse_number(Span::new("+inf.0"))
        );
        compare!(
            Sexpr::Float(std::f64::NEG_INFINITY),
            parse_number(Span::new("-inf.0"))
        );
        fail!(parse_number(Span::new("1x2y3")))
    }

    #[test]
    fn sexpr_number_parsing() {
        compare!(Sexpr::Integer(42), parse_sexpr(Span::new("42")));
        compare!(Sexpr::Integer(-24), parse_sexpr(Span::new("-24")));
        compare!(Sexpr::Float(3.1415), parse_sexpr(Span::new("3.1415")));
        fail!(parse_sexpr(Span::new("1x2y3")))
    }

    #[test]
    fn string_parsing() {
        compare!(
            Sexpr::String("42 )(foo-bar)"),
            parse_string(Span::new("\"42 )(foo-bar)\""))
        );
    }

    #[test]
    fn quotation_parsing() {
        compare!(
            vec![Sexpr::Symbol("quote"), Sexpr::Symbol("abc")],
            parse_abbreviation(Span::new("'abc"))
        );
    }

    #[test]
    fn quasiquotation_parsing() {
        compare!(
            vec![Sexpr::Symbol("quasiquote"), Sexpr::Symbol("abc")],
            parse_abbreviation(Span::new("`abc"))
        );
    }

    #[test]
    fn unquotation_parsing() {
        compare!(
            vec![Sexpr::Symbol("unquote"), Sexpr::Symbol("abc")],
            parse_abbreviation(Span::new(",abc"))
        );
    }

    #[test]
    fn splicing_unquotation_parsing() {
        compare!(
            vec![Sexpr::Symbol("unquote-splicing"), Sexpr::Symbol("abc")],
            parse_abbreviation(Span::new(",@abc"))
        );
    }

    #[test]
    fn comment_parsing() {
        compare!(
            vec![Sexpr::Symbol("x"), Sexpr::Symbol("y"), Sexpr::Symbol("z")],
            parse_sexpr(Span::new(
                "; preceding comment\n (x ; inner comment\n y z)  ; trailing comment\n"
            ))
        );
    }

    #[test]
    fn sexpr_parsing() {
        let x = Span::new("(abc 123 (4.5 \"y\" . z))");
        parse_sexpr(x).unwrap();
    }
}
