use chumsky::prelude::*;
use crate::token::Token;

type Error = Simple<Token>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Expression {}

pub fn expression_parser() -> impl Parser<Token, Expression, Error = Error> + Clone {
    unimplemented!()
}
