use crate::{
    expression::{expression_parser, Expression},
    token::Token,
};
use chumsky::prelude::*;
type Error = Simple<Token>;

type Designator = Expression;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ScalarType {
    Bit(Option<Designator>),
    Int(Option<Designator>),
    UInt(Option<Designator>),
    Float(Option<Designator>),
    Angle(Option<Designator>),
    Bool,
    Duration,
    Stretch,
    Complex(Option<Box<ScalarType>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType {
    scalar_type: ScalarType,
    expressions: Vec<Expression>,
}

pub type QubitType = Option<Designator>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IO {
    Input,
    Output,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OldStyleRegister {
    CReg,
    QReg,
}

pub fn scalar_type_parser() -> impl Parser<Token, ScalarType, Error = Error> {
    recursive(|scalar_type| {
        choice((
            just(Token::Bit)
                .ignore_then(expression_parser().or_not())
                .map(ScalarType::Bit),
            just(Token::Int)
                .ignore_then(expression_parser().or_not())
                .map(ScalarType::Int),
            just(Token::Uint)
                .ignore_then(expression_parser().or_not())
                .map(ScalarType::UInt),
            just(Token::Float)
                .ignore_then(expression_parser().or_not())
                .map(ScalarType::Float),
            just(Token::Angle)
                .ignore_then(expression_parser().or_not())
                .map(ScalarType::Angle),
            just(Token::Bool).map(|_| ScalarType::Bool),
            just(Token::Duration).map(|_| ScalarType::Duration),
            just(Token::Stretch).map(|_| ScalarType::Stretch),
            just(Token::Complex)
                .ignore_then(
                    /* Option<ScalarType */
                    just(Token::LBracket)
                        .ignore_then(scalar_type)
                        .then_ignore(just(Token::RBracket))
                        .or_not(),
                )
                .map(|opt_inner| ScalarType::Complex(opt_inner.map(Box::new))),
        ))
    })
}

pub fn array_type_parser() -> impl Parser<Token, ArrayType, Error = Error> {
    just(Token::Array)
        .ignore_then(just(Token::LBracket))
        .ignore_then(scalar_type_parser())
        .then_ignore(just(Token::Comma))
        .then(expression_parser().separated_by(just(Token::Comma)))
        .then_ignore(just(Token::RBracket))
        .map(|(scalar_type, expressions)| ArrayType {
            scalar_type,
            expressions,
        })
}
