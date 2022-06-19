use chumsky::prelude::*;
use crate::{token::Token, expression::{Expression, expression_parser}};
type Error = Simple<Token>;

type Identifier = String;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope {}
type Designator = Expression;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeclarationExpression {}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GateModifier {}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexedIdentifier {}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HardwareQubit {}

type GateOperand = IndexedIdentifier;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArgumentDefinition {
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExternArgument {
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MeasureExpression {
    Measure(GateOperand),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Pragma,
    AliasDeclaration(Identifier, Vec<Expression>),
    Assignment(IndexedIdentifier, Token /* EQUALS | CompoundAssignmentOperator */, MeasureExpression),
    Barrier(Vec<GateOperand>),
    Box(Option<Designator>),
    Break,
    CalibrationGrammer(String),
    ClassicalDeclaration(ArrayType, Identifier, DeclarationExpression),
    ConstDeclaration(ScalarType, Identifier, DeclarationExpression),
    Continue,
    Def(Identifier, Vec<ArgumentDefinition>, ScalarType),
    Defcal(Identifier, Vec<ArgumentDefinition>, Vec<HardwareQubit>, Option<ScalarType>),
    Delay(Designator, Vec<GateOperand>),
    End,
    Expression(Expression),
    Extern(Identifier, Vec<ExternArgument>, ScalarType),
    For /*  FOR scalarType Identifier IN (setExpression | LBRACKET rangeExpression RBRACKET | Identifier) body=statementOrScope; */,
    GateCall(Vec<GateModifier>, String, Vec<Expression>, Option<Designator>, Vec<GateOperand>),
    Gate(Identifier, Vec<Identifier>, Vec<Identifier>, Scope),
    If(Expression, Scope),
    Include(String),
    IoDeclaration(IO, ArrayType, Identifier),
    MeasureArrowAssignment(GateOperand, Option<IndexedIdentifier>),
    OldStyleDeclaration(OldStyleRegister, Identifier, Option<Designator>),
    QuantumDeclaration(QubitType, Identifier),
    Reset(GateOperand),
    Return(MeasureExpression),
    While(Expression, Scope),
}

pub fn statement_parser() -> impl Parser<Token, Statement, Error = Error> + Clone {
    // TODO: pragma, annotation, openpulse insts
    choice((
        just(Token::Qubit).ignore_then(expression_parser().or_not())
                          .then(select!{Token::Identifier(ident) => ident})
                          .then_ignore(just(Token::Semicolon))
                          .map(|(designator, ident)| Statement::QuantumDeclaration(designator, ident)),
        //just(Token::Const).ignore_then()
        //                  .then(select!{Token::Identifier(ident) => ident})
        just(Token::Let).ignore_then(select!{Token::Identifier(ident) => ident})
                        .then_ignore(just(Token::Equals))
                        .then(expression_parser().repeated().separated_by(just(Token::DoublePlus)))
                        .map(|(ident, exprs)| Statement::AliasDeclaration(ident, exprs)),
    ))
}
