use crate::token::{Token, Span};
use crate::error::Error;
use chumsky::prelude::*;

struct Program {
    version: Option<String>,
    statements: Vec<Statement>,
}

type Identifier = String;
struct Scope {}
enum Expression {}
type Designator = Expression;
struct AliasExpression {}
struct DeclarationExpression {}
struct GateModifier {}
struct IndexedIdentifier {}
struct HardwareQubit {}

type GateOperand = IndexedIdentifier;

enum ScalarType {
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

struct ArrayType {
    scalar_type: ScalarType,
    expressions: Vec<Expression>,
}

enum QubitType {
}

enum IO {
    Input,
    Output,
}

enum OldStyleRegister {
    CReg,
    QReg,
}

enum ArgumentDefinition {
}

enum ExternArgument {
}

enum MeasureExpression {
    Measure(GateOperand),
    Expression(Expression),
}

enum Statement {
    Pragma,
    AliasDeclaration(Identifier, AliasExpression),
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

fn parser() -> impl Parser<Token, Program, Error = Error> {
    just(Token::Openqasm)
}
