use crate::{token::Token, statement::Statement};
use chumsky::prelude::*;

pub type Error = Simple<Token>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Program {
    version: Option<String>,
    statements: Vec<Statement>,
}

pub fn parser() -> impl Parser<Token, Program, Error = Error> {
    select! {
        Token::Openqasm(ver) => ver,
    }.then_ignore(just(Token::Semicolon)).or_not().map(|version|
        Program {
            version,
            statements: vec![],
        })
}

#[cfg(test)]
mod tests {
    use super::{parser, Program, Token};
    use chumsky::Parser;

    #[test]
    fn program_parser_empty() {
        assert_eq!(
            parser().parse(vec![]),
            Ok(Program {
                version: None,
                statements: vec![]
            })
        );
    }

    #[test]
    fn program_parser_version() {
        assert_eq!(
            parser().parse(vec![Token::Openqasm("3".to_owned()), Token::Semicolon]),
            Ok(Program {
                version: Some("3".to_owned()),
                statements: vec![]
            })
        );
    }
}
