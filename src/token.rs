use chumsky::prelude::*;
use ordered_float::OrderedFloat;
use std::cmp::PartialEq;
use std::convert::From;
use std::ops::Range;

pub type Span = Range<usize>;
pub type Error = Simple<char>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    // Language keywords
    Openqasm(String),
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
    FloatLiteral(OrderedFloat<f64>),
    Identifier(String),
    // TimeUnitLiteral(TimeUnit),
    BitstringLiteral(String),
    StringLiteral(String),
    // Syntax Error
    SyntaxError(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinTimingInstructionToken {
    Delay,
    Rotary,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EqualityOperatorToken {
    Eq,
    Neq,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ComparisonOperatorToken {
    GT,
    LT,
    GE,
    LE,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BitshiftOperatorToken {
    LShift,
    RShift,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant {
    Pi,
    Tau,
    Euler,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Number {
    Int(i64),
    Float(OrderedFloat<f64>),
}

impl From<Number> for f64 {
    fn from(number: Number) -> Self {
        match number {
            Number::Int(x) => x as f64,
            Number::Float(x) => *x,
        }
    }
}

/*
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TimeUnit {
    DT,
    NanoSec,
    MicroSec,
    MilliSec,
    Sec,
}
*/

fn comment() -> impl Parser<char, (), Error = Error> + Clone {
    choice((
        just("//").then(take_until(just('\n'))).ignored(),
        just("//").then(take_until(end())).ignored(),
        just("/*").then(take_until(just("*/"))).ignored(),
    ))
    .padded()
}

fn keyword_or_ident() -> impl Parser<char, Token, Error = Error> + Clone {
    text::ident()
        .map(|s: String| match s.as_str() {
            "include" => Token::Include,
            "defcalgrammar" => Token::Defcalgrammar,
            "defcal" => Token::Defcal,
            "def" => Token::Def,
            "gate" => Token::Gate,
            "extern" => Token::Extern,
            "box" => Token::Box,
            "let" => Token::Let,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "else" => Token::Else,
            "end" => Token::End,
            "return" => Token::Return,
            "for" => Token::For,
            "while" => Token::While,
            "input" => Token::Input,
            "output" => Token::Output,
            "const" => Token::Const,
            "mutable" => Token::Mutable,
            "qreg" => Token::Qreg,
            "qubit" => Token::Qubit,
            "creg" => Token::Creg,
            "bool" => Token::Bool,
            "bit" => Token::Bit,
            "int" => Token::Int,
            "uint" => Token::Uint,
            "float" => Token::Float,
            "angle" => Token::Angle,
            "complex" => Token::Complex,
            "array" => Token::Array,
            "durationof" => Token::Durationof,
            "duration" => Token::Duration,
            "stretch" => Token::Stretch,
            "gphase" => Token::Gphase,
            "inv" => Token::Inv,
            "pow" => Token::Pow,
            "negctrl" => Token::Negctrl,
            "ctrl" => Token::Ctrl,
            "sizeof" => Token::Sizeof,
            "reset" => Token::Reset,
            "measure" => Token::Measure,
            "barrier" => Token::Barrier,
            "delay" => Token::BuiltinTimingInstruction(BuiltinTimingInstructionToken::Delay),
            "rotary" => Token::BuiltinTimingInstruction(BuiltinTimingInstructionToken::Rotary),
            "true" => Token::BooleanLiteral(true),
            "false" => Token::BooleanLiteral(false),
            "arccos" => Token::BuiltinMath(BuiltinMathToken::Arccos),
            "arcsin" => Token::BuiltinMath(BuiltinMathToken::Arcsin),
            "arctan" => Token::BuiltinMath(BuiltinMathToken::Arctan),
            "cos" => Token::BuiltinMath(BuiltinMathToken::Cos),
            "exp" => Token::BuiltinMath(BuiltinMathToken::Exp),
            "ln" => Token::BuiltinMath(BuiltinMathToken::Ln),
            "popcount" => Token::BuiltinMath(BuiltinMathToken::Popcount),
            "rotl" => Token::BuiltinMath(BuiltinMathToken::Rotl),
            "rotr" => Token::BuiltinMath(BuiltinMathToken::Rotr),
            "sin" => Token::BuiltinMath(BuiltinMathToken::Sin),
            "sqrt" => Token::BuiltinMath(BuiltinMathToken::Sqrt),
            "tan" => Token::BuiltinMath(BuiltinMathToken::Tan),
            "pi" => Token::ConstantLiteral(Constant::Pi),
            "tau" => Token::ConstantLiteral(Constant::Tau),
            "euler" => Token::ConstantLiteral(Constant::Euler),
            // "dt" => Token::TimeUnitLiteral(TimeUnit::DT),
            // "ns" => Token::TimeUnitLiteral(TimeUnit::NanoSec),
            // "us" => Token::TimeUnitLiteral(TimeUnit::MicroSec),
            // "ms" => Token::TimeUnitLiteral(TimeUnit::MilliSec),
            // "s" => Token::TimeUnitLiteral(TimeUnit::Sec),
            "if" => Token::If,
            "im" => Token::Imag,
            "in" => Token::In,
            "CX" => Token::CX,
            "U" => Token::U,
            _ => Token::Identifier(s),
        })
        .or(just('#').ignore_then(text::ident().validate(
            |s: String, span, emit| match s.as_str() {
                "pragma" => Token::Pragma,
                "dim" => Token::Dim,
                _ => {
                    emit(Error::expected_input_found(span, None, s.chars().next()));
                    Token::SyntaxError(s)
                }
            },
        )))
        .or(choice((
            just("π").to(Token::ConstantLiteral(Constant::Pi)),
            just("τ").to(Token::ConstantLiteral(Constant::Tau)),
            just("ℇ").to(Token::ConstantLiteral(Constant::Euler)),
            // just("μs").to(Token::TimeUnitLiteral(TimeUnit::MicroSec)),
        )))
}

fn puncts() -> impl Parser<char, Token, Error = Error> + Clone {
    choice((
        just("<<=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::LShiftEq,
        )),
        just(">>=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::RShiftEq,
        )),
        just("**=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::DoubleAsteriskEq,
        )),
        just("+=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::PlusEq,
        )),
        just("-=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::MinusEq,
        )),
        just("*=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::AsteriskEq,
        )),
        just("/=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::SlashEq,
        )),
        just("&=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::AmpersandEq,
        )),
        just("|=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::PipeEq,
        )),
        just("~=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::TildeEq,
        )),
        just("^=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::CaretEq,
        )),
        just("%=").to(Token::CompoundAssignmentOperator(
            CompoundAssignmentOperatorToken::PercentEq,
        )),
    ))
    .or(choice((
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
        just("[").to(Token::LBracket),
        just("]").to(Token::RBracket),
        just("{").to(Token::LBrace),
        just("}").to(Token::RBrace),
        just("(").to(Token::LParen),
        just(")").to(Token::RParen),
    )))
    .or(choice((
        just("+").to(Token::Plus),
        just("-").to(Token::Minus),
        just("*").to(Token::Asterisk),
        just("/").to(Token::Slash),
        just("%").to(Token::Percent),
        just("|").to(Token::Pipe),
        just("&").to(Token::Ampersand),
        just("^").to(Token::Caret),
        just("~").to(Token::Tilde),
        just("@").to(Token::At),
        just("!").to(Token::ExclamationPoint),
        just("=").to(Token::Equals),
        just(":").to(Token::Colon),
        just(";").to(Token::Semicolon),
        just(".").to(Token::Dot),
        just(",").to(Token::Comma),
    )))
}

fn integer_literal() -> impl Parser<char, Token, Error = Error> + Clone {
    choice((
        text::int(10).map(|s: String| s.parse()),
        just("0x")
            .or(just("0X"))
            .ignore_then(text::int(16).map(|s: String| i64::from_str_radix(&s, 16))),
        just("0o").ignore_then(text::int(8).map(|s: String| i64::from_str_radix(&s, 8))),
        just("0b")
            .or(just("0B"))
            .ignore_then(text::int(2).map(|s: String| i64::from_str_radix(&s, 2))),
    ))
    .map(|res| Token::IntegerLiteral(res.expect("Failed to convert to i64")))
}

fn string_literal() -> impl Parser<char, Token, Error = Error> + Clone {
    // TODO: escape sequence
    just('"')
        .ignore_then(take_until(just('"')))
        .map(|(cs, _)| Token::StringLiteral(cs.into_iter().collect::<String>()))
}

fn float_literal() -> impl Parser<char, Token, Error = Error> + Clone {
    let exponent = just('e')
        .or(just('E'))
        .ignore_then(just('+').or(just('-')).or_not().map(|c| c.unwrap_or('+')))
        .then(text::int(10))
        .map(|(a, b)| format!("e{}{}", a, b));
    // TODO: .1e3
    choice((
        text::int(10)
            .then(exponent)
            .map(|(a, b)| format!("{}e{}", a, b)),
        just('.')
            .ignore_then(text::int(10))
            .map(|a| format!(".{}", a)),
        text::int(10)
            .then_ignore(just('.'))
            .then(text::int(10).or_not())
            .then(exponent.or_not())
            .map(|((a, b), c)| format!("{}.{}{}", a, b.unwrap_or_default(), c.unwrap_or_default())),
    ))
    .map(|s| Token::FloatLiteral(s.parse().expect("Failed to parse float")))
}

fn openqasm_ver() -> impl Parser<char, Token, Error = Error> + Clone {
    just("OPENQASM")
        .padded()
        .ignore_then(text::int(10).separated_by(just('.')))
        .map(|v| Token::Openqasm(v.join(".")))
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Error> {
    openqasm_ver()
        .or(keyword_or_ident())
        .or(float_literal())
        .or(integer_literal())
        .or(string_literal())
        .or(puncts())
        .padded_by(comment().repeated())
        .padded()
        .map_with_span(move |token, span| (token, span))
        .repeated()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::{lexer, Constant, Token};
    use chumsky::Parser;
    use std::fs;

    #[test]
    fn lexer1() {
        assert_eq!(
            lexer()
                .parse("OPENQASM qubit#dimπU/**//*aa */foo//   comment")
                .map(|v| v.into_iter().map(|(token, _)| token).collect()),
            Ok(vec![
                Token::Openqasm(String::new()),
                Token::Qubit,
                Token::Dim,
                Token::ConstantLiteral(Constant::Pi),
                Token::U,
                Token::Identifier("foo".to_string()),
            ])
        );
    }

    #[test]
    fn lexer_openqasm_ver0() {
        assert_eq!(
            lexer()
                .parse("OPENQASM if")
                .map(|v| v.into_iter().map(|(token, _)| token).collect()),
            Ok(vec![Token::Openqasm("".to_owned()), Token::If,])
        );
    }

    #[test]
    fn lexer_openqasm_ver2() {
        assert_eq!(
            lexer()
                .parse("OPENQASM 3.0 1")
                .map(|v| v.into_iter().map(|(token, _)| token).collect()),
            Ok(vec![
                Token::Openqasm("3.0".to_owned()),
                Token::IntegerLiteral(1),
            ])
        );
    }

    #[test]
    fn lexer_openqasm_ver3() {
        assert_eq!(
            lexer()
                .parse("OPENQASM 3.0.1 1")
                .map(|v| v.into_iter().map(|(token, _)| token).collect()),
            Ok(vec![
                Token::Openqasm("3.0.1".to_owned()),
                Token::IntegerLiteral(1),
            ])
        );
    }

    #[test]
    fn lexer_read_unknown_punkt_is_error() {
        assert!(lexer().parse("while $ if").is_err());
    }

    #[test]
    fn lexer_works_without_error_qelib1() {
        let qasm = fs::read_to_string("qasm-examples/generic/qelib1.inc")
            .expect("Sample QASM file not found.");
        let (tokens, errors) = lexer().parse_recovery_verbose(qasm.as_str());
        eprintln!("tokens: {:?}", tokens);
        eprintln!("errors: {:?}", errors);
        let qasm_chars = qasm.chars().collect::<Vec<_>>();
        for e in errors.iter() {
            eprintln!(
                "span: {:?}, file: {:?}",
                e.span(),
                qasm_chars[e.span()].iter().collect::<Vec<_>>()
            );
        }
        assert!(errors.is_empty());
    }

    #[test]
    fn lexer_works_without_error_qe_qft_5() {
        let qasm = fs::read_to_string("qasm-examples/ibmqx2/qe_qft_5.qasm")
            .expect("Sample QASM file not found.");
        let (tokens, errors) = lexer().parse_recovery_verbose(qasm.as_str());
        eprintln!("tokens: {:?}", tokens);
        eprintln!("errors: {:?}", errors);
        let qasm_chars = qasm.chars().collect::<Vec<_>>();
        for e in errors.iter() {
            eprintln!(
                "span: {:?}, file: {:?}",
                e.span(),
                qasm_chars[e.span()].iter().collect::<Vec<_>>()
            );
        }
        assert!(errors.is_empty());
    }
}
