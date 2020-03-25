use crate::span::{Span, Spanned};

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
    Nil,
    True,
    False,
    Char(char),
    Symbol(&'a str),
    String(&'a str),
    Integer(i64),
    Float(f64),
    List(Vec<SpannedSexpr<'a>>),
    Vector(Vec<SpannedSexpr<'a>>),

    Dot,
}

impl<'a> Spanned<'a> for SpannedSexpr<'a> {
    fn span(&self) -> &Span<'a> {
        &self.span
    }
    fn span_mut(&mut self) -> &mut Span<'a> {
        &mut self.span
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

pub trait IntoSpannedSexpr<'a> {
    fn into_spanned(self, expr: Sexpr<'a>) -> SpannedSexpr<'a>;
}

impl<'a> IntoSpannedSexpr<'a> for Span<'a> {
    fn into_spanned(self, expr: Sexpr<'a>) -> SpannedSexpr<'a> {
        SpannedSexpr { span: self, expr }
    }
}
