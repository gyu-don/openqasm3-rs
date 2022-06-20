use crate::token::Token;
use chumsky::prelude::*;

type Error = Simple<Token>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Dummy,
}

pub fn expression_parser() -> impl Parser<Token, Expression, Error = Error> + Clone {
    empty().map(|_| Expression::Dummy)
}
