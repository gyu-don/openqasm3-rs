use crate::token::Token;
use chumsky::prelude::*;

type Error = Simple<Token>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Dummy,
}

pub fn expression_parser() -> impl Parser<Token, Expression, Error = Error> + Clone {
    just(Token::Semicolon).map(|_| Expression::Dummy)
}
