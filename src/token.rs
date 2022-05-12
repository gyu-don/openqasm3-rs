use crate::error::Error;
use chumsky::prelude::*;
use std::cmp::PartialEq;
use std::convert::From;
use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Language keywords
    Openqasm,
    Include,
    Pragma,
    Defcalgrammar,
    Def,
    Defcal,
    Gate,
    Extern,
    Box,
    Let,
    Break,
    Continue,
    If,
    Else,
    End,
    Return,
    For,
    While,
    In,
    // Types
    Input,
    Output,
    Const,
    Mutable,
    Qreg,
    Qubit,
    Creg,
    Bool,
    Bit,
    Int,
    Uint,
    Float,
    Angle,
    Complex,
    Array,
    Duration,
    Stretch,
    // Builtin identifiers and operations
    U,
    CX,
    Gphase,
    Inv,
    Pow,
    Ctrl,
    Negctrl,
    Dim,
    Sizeof,
    BuiltinMath(BuiltinMathToken),
    Durationof,
    BuiltinTimingInstruction(BuiltinTimingInstructionToken),
    Reset,
    Measure,
    Barrier,
    BooleanLiteral(bool),
    // Symbols
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Colon,
    Semicolon,
    Dot,
    Comma,
    Equals,
    Arrow,
    Plus,
    DoublePlus,
    Minus,
    Asterisk,
    DoubleAsterisk,
    Slash,
    Percent,
    Pipe,
    DoublePipe,
    Ampersand,
    DoubleAmpersand,
    Caret,
    At,
    Tilde,
    ExclamationPoint,
    EqualityOperator(EqualityOperatorToken),
    CompoundAssignmentOperator(CompoundAssignmentOperatorToken),
    ComparisonOperator(ComparisonOperatorToken),
    BitshiftOperator(BitshiftOperatorToken),
    Imag,
    ConstantLiteral(Constant),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    Identifier(String),
    TimeUnitLiteral(TimeUnit),
    BitstringLiteral(String),
    StringLiteral(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinMathToken {
    Arccos,
    Arcsin,
    Arctan,
    Cos,
    Exp,
    Ln,
    Popcount,
    Rotl,
    Rotr,
    Sin,
    Sqrt,
    Tan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinTimingInstructionToken {
    Delay,
    Rotary,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EqualityOperatorToken {
    Eq,
    Neq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompoundAssignmentOperatorToken {
    PlusEq,
    MinusEq,
    AsteriskEq,
    SlashEq,
    AmpersandEq,
    PipeEq,
    TildeEq,
    CaretEq,
    LShiftEq,
    RShiftEq,
    PercentEq,
    DoubleAsteriskEq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComparisonOperatorToken {
    GT,
    LT,
    GE,
    LE,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BitshiftOperatorToken {
    LShift,
    RShift,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constant {
    Pi,
    Tau,
    Euler,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl From<Number> for f64 {
    fn from(number: Number) -> Self {
        match number {
            Number::Int(x) => x as f64,
            Number::Float(x) => x,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TimeUnit {
    DT,
    NanoSec,
    MicroSec,
    MilliSec,
    Sec,
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Error> {
    let const_keyword = choice((
        just("OPENQASM").to(Token::Openqasm),
        just("include").to(Token::Include),
        just("#pragma").to(Token::Pragma),
        just("defcalgrammar").to(Token::Defcalgrammar),
        just("defcal").to(Token::Defcal),
        just("def").to(Token::Def),
        just("gate").to(Token::Gate),
        just("extern").to(Token::Extern),
        just("box").to(Token::Box),
        just("let").to(Token::Let),
        just("break").to(Token::Break),
        just("continue").to(Token::Continue),
        just("if").to(Token::If),
        just("else").to(Token::Else),
        just("end").to(Token::End),
        just("return").to(Token::Return),
        just("for").to(Token::For),
        just("while").to(Token::While),
        just("input").to(Token::Input),
        just("output").to(Token::Output),
        just("in").to(Token::In),
    ))
    .or(choice((
        just("const").to(Token::Const),
        just("mutable").to(Token::Mutable),
        just("qreg").to(Token::Qreg),
        just("qubit").to(Token::Qubit),
        just("creg").to(Token::Creg),
        just("bool").to(Token::Bool),
        just("bit").to(Token::Bit),
        just("int").to(Token::Int),
        just("uint").to(Token::Uint),
        just("float").to(Token::Float),
        just("angle").to(Token::Angle),
        just("complex").to(Token::Complex),
        just("array").to(Token::Array),
        just("durationof").to(Token::Durationof),
        just("duration").to(Token::Duration),
        just("stretch").to(Token::Stretch),
    )))
    .or(choice((
        just("gphase").to(Token::Gphase),
        just("inv").to(Token::Inv),
        just("pow").to(Token::Pow),
        just("negctrl").to(Token::Negctrl),
        just("ctrl").to(Token::Ctrl),
        just("#dim").to(Token::Dim),
        just("sizeof").to(Token::Sizeof),
    ))).or(choice((
        just("arccos").to(Token::BuiltinMath(BuiltinMathToken::Arccos)),
        just("arcsin").to(Token::BuiltinMath(BuiltinMathToken::Arcsin)),
        just("arctan").to(Token::BuiltinMath(BuiltinMathToken::Arctan)),
        just("cos").to(Token::BuiltinMath(BuiltinMathToken::Cos)),
        just("exp").to(Token::BuiltinMath(BuiltinMathToken::Exp)),
        just("ln").to(Token::BuiltinMath(BuiltinMathToken::Ln)),
        just("popcount").to(Token::BuiltinMath(BuiltinMathToken::Popcount)),
        just("rotl").to(Token::BuiltinMath(BuiltinMathToken::Rotl)),
        just("rotr").to(Token::BuiltinMath(BuiltinMathToken::Rotr)),
        just("sin").to(Token::BuiltinMath(BuiltinMathToken::Sin)),
        just("sqrt").to(Token::BuiltinMath(BuiltinMathToken::Sqrt)),
        just("tan").to(Token::BuiltinMath(BuiltinMathToken::Tan)),
    ))).or(choice((
        just("delay").to(Token::BuiltinTimingInstruction(
            BuiltinTimingInstructionToken::Delay,
        )),
        just("rotary").to(Token::BuiltinTimingInstruction(
            BuiltinTimingInstructionToken::Rotary,
        )),
        just("reset").to(Token::Reset),
        just("measure").to(Token::Measure),
        just("Barrier").to(Token::Barrier),
        just("true").to(Token::BooleanLiteral(true)),
        just("false").to(Token::BooleanLiteral(false)),
        just("pi").to(Token::ConstantLiteral(Constant::Pi)),
        just("π").to(Token::ConstantLiteral(Constant::Pi)),
        just("tau").to(Token::ConstantLiteral(Constant::Tau)),
        just("τ").to(Token::ConstantLiteral(Constant::Tau)),
        just("euler").to(Token::ConstantLiteral(Constant::Euler)),
        just("ℇ").to(Token::ConstantLiteral(Constant::Euler)),
        just("CX").to(Token::CX),
        just("U").to(Token::U),
    ))).or(choice((
        just("<<=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::LShiftEq)),
        just(">>=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::RShiftEq)),
        just("**=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::DoubleAsteriskEq)),
        just("+=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::PlusEq)),
        just("-=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::MinusEq)),
        just("*=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::AsteriskEq)),
        just("/=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::SlashEq)),
        just("&=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::AmpersandEq)),
        just("|=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::PipeEq)),
        just("~=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::TildeEq)),
        just("^=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::CaretEq)),
        just("%=").to(Token::CompoundAssignmentOperator(CompoundAssignmentOperatorToken::PercentEq)),
        just("++").to(Token::DoublePlus),
        just("**").to(Token::DoubleAsterisk),
        just("||").to(Token::DoublePipe),
        just("&&").to(Token::DoubleAmpersand),
        just(">>").to(Token::BitshiftOperator(BitshiftOperatorToken::RShift)),
        just("<<").to(Token::BitshiftOperator(BitshiftOperatorToken::LShift)),
        just("==").to(Token::EqualityOperator(EqualityOperatorToken::Eq)),
        just("!=").to(Token::EqualityOperator(EqualityOperatorToken::Neq)),
        just(">=").to(Token::ComparisonOperator(ComparisonOperatorToken::GE)),
        just("<=").to(Token::ComparisonOperator(ComparisonOperatorToken::LE)),
        just(">").to(Token::ComparisonOperator(ComparisonOperatorToken::GT)),
        just("<").to(Token::ComparisonOperator(ComparisonOperatorToken::LT)),
    )));
    const_keyword
        .padded()
        .map_with_span(move |token, span| (token, span))
        .repeated()
}
