use crate::{
    directive::Directive, expression::Expression, identifier::Identifier, instruction::Instruction,
    token::Token,
};
use logos::{Lexer, Logos};
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {
    #[error("Byte literal out of range: {0:?}")]
    ByteLiteralOutOfRange(Token),
    #[error("Duplicate section directive at {0}")]
    DuplicateSectionDirective(codespan::Span),
    #[error("End of file")]
    Eof,
    #[error("Half literal out of range: {0:?}")]
    HalfLiteralOutOfRange(Token),
    #[error("Incorrect literal type '{expected:?}'. Expected '{expected:?}'")]
    IncorrectLiteralType { got: Token, expected: &'static str },
    #[error("Got register for CP{got} where register for CP{expected} was expected at {token:?}")]
    IncorrectCoprocRegister {
        got: &'static str,
        expected: &'static str,
        token: Token,
    },
    #[error("Invalid alignment '{0:?}'. Alignment values may only be in [0, 3].")]
    InvalidAlignment(Token),
    #[error("Invalid float literal '{0:?}'")]
    InvalidFloatLiteral(Token),
    #[error("Invalid integer literal '{0:?}'")]
    InvalidIntegerLiteral(Token),
    #[error("Lexer error at {0}")]
    LexerError(codespan::Span),
    #[error("Missing comma at {0}")]
    MissingComma(codespan::Span),
    #[error("Got unexpected token '{got:?}'. Expected '{expected:?}")]
    UnexpectedToken { got: Token, expected: &'static str },
    #[error("Undefined label '{0}' at {1}")]
    UnknownLabel(String, codespan::Span),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Section {
    Data,
    Text,
}

pub struct Parser<'lex> {
    pub lexer: Lexer<'lex, Token>,
    section: Section,
    expressions: Vec<Expression>,
    defined_labels: Vec<String>,
}

impl<'lex> Parser<'lex> {
    pub fn new(s: &'lex str) -> Parser<'lex> {
        Parser {
            lexer: Token::lexer(s),
            section: Section::Data,
            expressions: Vec::new(),
            defined_labels: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Expression>, Error> {
        // The first non-comment item must be a text or data section directive.
        match self.lexer.next().ok_or(Error::Eof)? {
            Token::AssemblerDirective(directive) => match directive {
                Directive::DataSection => {
                    self.section = Section::Data;
                    self.expressions
                        .push(Expression::Section(Directive::DataSection));
                    Ok(())
                }
                Directive::TextSection => {
                    self.section = Section::Text;
                    self.expressions
                        .push(Expression::Section(Directive::TextSection));
                    Ok(())
                }
                other => Err(Error::UnexpectedToken {
                    got: Token::AssemblerDirective(other),
                    expected: "data or text section directive",
                }),
            },
            other => Err(Error::UnexpectedToken {
                got: other,
                expected: "data or text section directive",
            }),
        }?;
        // Collect all expressions.
        self.first_pass()?;
        // Check that all labels are valid.
        self.second_pass()?;
        // Return expressions.
        Ok(self.expressions)
    }

    fn first_pass(&mut self) -> Result<(), Error> {
        while let Some(first) = self.lexer.next() {
            println!("parsing: {:?}", first);
            self.expressions.push(match first {
                Token::AssemblerDirective(directive) => match directive {
                    Directive::DataSection => {
                        if self.section == Section::Text {
                            self.section = Section::Data;
                            Ok(Expression::Section(directive))
                        } else {
                            Err(Error::DuplicateSectionDirective(codespan::Span::new(
                                self.lexer.span().start as u32,
                                self.lexer.span().end as u32,
                            )))
                        }
                    }
                    Directive::TextSection => {
                        if self.section == Section::Data {
                            self.section = Section::Text;
                            Ok(Expression::Section(directive))
                        } else {
                            Err(Error::DuplicateSectionDirective(codespan::Span::new(
                                self.lexer.span().start as u32,
                                self.lexer.span().end as u32,
                            )))
                        }
                    }
                    Directive::GlobalSymbol => Directive::parse(directive, &mut self.lexer),
                    _ => match self.section {
                        Section::Data => Directive::parse(directive, &mut self.lexer),
                        Section::Text => Err(Error::UnexpectedToken {
                            got: Token::AssemblerDirective(directive),
                            expected: "instruction or label declaration",
                        }),
                    },
                },
                Token::Identifier(ident) => match self.section {
                    Section::Data => Err(Error::UnexpectedToken {
                        got: Token::Identifier(ident),
                        expected: "assembler directive",
                    }),
                    Section::Text => match ident {
                        Identifier::Instruction(instr) => {
                            Instruction::parse(instr, &mut self.lexer)
                        }
                        label => Err(Error::UnexpectedToken {
                            got: Token::Identifier(label),
                            expected: "instruction or label definition",
                        }),
                    },
                },
                Token::LabelDefinition(label) => {
                    self.defined_labels.push(label.clone());
                    Ok(Expression::LabelDefinition(label))
                }
                other => Err(match self.section {
                    Section::Data => Error::UnexpectedToken {
                        got: other,
                        expected: "assembler directive",
                    },
                    Section::Text => Error::UnexpectedToken {
                        got: other,
                        expected: "instruction or label definition",
                    },
                }),
            }?);
        }
        Ok(())
    }

    fn second_pass(&mut self) -> Result<(), Error> {
        Ok(())
    }
}
