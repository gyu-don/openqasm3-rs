use crate::{
    expression::{expression_parser, Expression},
    token::Token,
    types::{array_type_parser, scalar_type_parser, ArrayType, QubitType, ScalarType},
};
use chumsky::prelude::*;
type Error = Simple<Token>;

type Identifier = String;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope {}
type Designator = Expression;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GateModifier {}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexedIdentifier {}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HardwareQubit {}

type GateOperand = IndexedIdentifier;

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
pub enum ArgumentDefinition {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExternArgument {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MeasureExpression {
    Measure(GateOperand),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArrayLiteral {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeclarationExpression {
    ArrayLiteral(ArrayLiteral),
    Measure(GateOperand),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Pragma,
    AliasDeclaration(Identifier, Vec<Expression>),
    Assignment(
        IndexedIdentifier,
        Token, /* EQUALS | CompoundAssignmentOperator */
        MeasureExpression,
    ),
    Barrier(Vec<GateOperand>),
    Box(Option<Designator>),
    Break,
    CalibrationGrammer(String),
    ClassicalDeclaration(ArrayType, Identifier, Option<DeclarationExpression>),
    ConstDeclaration(ScalarType, Identifier, DeclarationExpression),
    Continue,
    Def(Identifier, Vec<ArgumentDefinition>, ScalarType),
    Defcal(
        Identifier,
        Vec<ArgumentDefinition>,
        Vec<HardwareQubit>,
        Option<ScalarType>,
    ),
    Delay(Designator, Vec<GateOperand>),
    End,
    Expression(Expression),
    Extern(Identifier, Vec<ExternArgument>, ScalarType),
    For, /*  FOR scalarType Identifier IN (setExpression | LBRACKET rangeExpression RBRACKET | Identifier) body=statementOrScope; */
    GateCall(
        Vec<GateModifier>,
        String,
        Vec<Expression>,
        Option<Designator>,
        Vec<GateOperand>,
    ),
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
    let identifier = select! {Token::Identifier(ident) => ident};
    let declaration_expression_parser = || expression_parser().map(DeclarationExpression::Expression); // TODO: MeasureExpression | arrayLiteral
                                                                                             // TODO: pragma, annotation, openpulse insts
    choice((
        just(Token::Qubit)
            .ignore_then(expression_parser().or_not())
            .then(identifier)
            .then_ignore(just(Token::Semicolon))
            .map(|(designator, ident)| Statement::QuantumDeclaration(designator, ident)),
        just(Token::Const)
            .ignore_then(scalar_type_parser())
            .then(identifier)
            .then_ignore(just(Token::Equals))
            .then(declaration_expression_parser())
            .then_ignore(just(Token::Semicolon))
            .map(|((scalar_type, ident), decl_expr)| {
                Statement::ConstDeclaration(scalar_type, ident, decl_expr)
            }),
        just(Token::Let)
            .ignore_then(select! {Token::Identifier(ident) => ident})
            .then_ignore(just(Token::Equals))
            .then(expression_parser().separated_by(just(Token::DoublePlus)))
            .then_ignore(just(Token::Semicolon))
            .map(|(ident, exprs)| Statement::AliasDeclaration(ident, exprs)),
        array_type_parser()
            .then(identifier)
            .then(just(Token::Equals).ignore_then(declaration_expression_parser().or_not()))
            .then_ignore(just(Token::Semicolon))
            .map(|((array_type, ident), decl_expr)| {
                Statement::ClassicalDeclaration(array_type, ident, decl_expr)
            }),
        choice((just(Token::Input).map(|_| IO::Input), just(Token::Output).map(|_| IO::Output)))
            .then(array_type_parser())
            .then(identifier)
            .then_ignore(just(Token::Semicolon))
            .map(|((io, array_type), ident)| {
                Statement::IoDeclaration(io, array_type, ident)
            }),
        choice((just(Token::Qreg).map(|_| OldStyleRegister::QReg), just(Token::Creg).map(|_| OldStyleRegister::CReg)))
            .then(identifier)
            .then(expression_parser().or_not())
            .map(|((reg, ident), designator)| {
                Statement::OldStyleDeclaration(reg, ident, designator)
            }),
    ))
}
