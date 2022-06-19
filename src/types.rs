use chumsky::prelude::*;
use crate::{token::Token, expression::{Expression, expression_parser}};
type Error = Simple<Token>;

type Identifier = String;
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

type QubitType = Option<Designator>;

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
    choice(( 
        just(Token::Bit).ignore_then(expression_parser().or_not()).map(|designator| ScalarType::Bit(designator)),
        just(Token::Int).ignore_then(expression_parser().or_not()).map(|designator| ScalarType::Int(designator)),
        just(Token::Uint).ignore_then(expression_parser().or_not()).map(|designator| ScalarType::UInt(designator)),
        just(Token::Float).ignore_then(expression_parser().or_not()).map(|designator| ScalarType::Float(designator)),
        just(Token::Angle).ignore_then(expression_parser().or_not()).map(|designator| ScalarType::Angle(designator)),
        just(Token::Bool).map(|_| ScalarType::Bool),
        just(Token::Duration).map(|_| ScalarType::Duration),
        just(Token::Stretch).map(|_| ScalarType::Stretch),
        just(Token::Complex).ignore_then(
            just(Token::LBracket)
            .ignore_then(scalar_type_parser())
            .then_ignore(just(Token::RBracket))).map(|inner| Box::new(inner)).or_not().map(|scalar_type| ScalarType::Complex(scalar_type))
    ))
}
