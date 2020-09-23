use crate::{
    directive::Directive, expression::Expression, identifier::Identifier, instruction::Instruction,
    token::Token,
};
use logos::{Lexer, Logos, Span};
use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum Error {
    #[error("Byte literal out of range: {token:?} ({span:?})")]
    ByteLiteralOutOfRange { token: Token, span: Span },
    #[error("Duplicate section directive {0}")]
    DuplicateSectionDirective(codespan::Span),
    #[error("End of file")]
    Eof,
    #[error("Half literal out of range: {token:?} ({span:?})")]
    HalfLiteralOutOfRange { token: Token, span: Span },
    #[error("Incorrect literal type '{expected:?}'. Expected '{expected:?}'")]
    IncorrectLiteralType {
        got: Token,
        span: Span,
        expected: &'static str,
    },
    #[error("Register for CP{got} where register for CP{expected} was expected at {span:?}")]
    IncorrectCoprocRegister {
        got: &'static str,
        expected: &'static str,
        span: Span,
    },
    #[error("Invalid alignment '{token:?}' ({span:?}). Alignment values may only be in [0, 3].")]
    InvalidAlignment { token: Token, span: Span },
    #[error("Invalid float literal '{0:?}' ({1:?})")]
    InvalidFloatLiteral(Token, Span),
    #[error("Invalid integer literal '{0:?}' ({1:?})")]
    InvalidIntegerLiteral(Token, Span),
    #[error("Lexer error at {0}")]
    LexerError(codespan::Span),
    #[error("Missing comma at {0}")]
    MissingComma(codespan::Span),
    #[error("Got unexpected token '{got:?}' ({span:?}). Expected '{expected:?}")]
    UnexpectedToken {
        got: Token,
        span: Span,
        expected: &'static str,
    },
    #[error("Undefined label '{0}' at {1}")]
    UnknownLabel(String, codespan::Span),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Section {
    Data,
    Text,
}

pub struct Parser<'lex> {
    pub(crate) lexer: Lexer<'lex, Token>,
    pub(crate) carry: Option<Token>,
    section: Section,
    expressions: Vec<Expression>,
    defined_labels: Vec<String>,
}

impl<'lex> Parser<'lex> {
    pub fn new(s: &'lex str) -> Parser<'lex> {
        Parser {
            lexer: Token::lexer(s),
            carry: None,
            section: Section::Data,
            expressions: Vec::new(),
            defined_labels: Vec::new(),
        }
    }

    pub(crate) fn next(&mut self) -> Option<Token> {
        self.carry.take().or_else(|| self.lexer.next())
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
                    span: self.lexer.span(),
                    expected: "data or text section directive",
                }),
            },
            other => Err(Error::UnexpectedToken {
                got: other,
                span: self.lexer.span(),
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
        while let Some(first) = self.next() {
            let expression = match first {
                Token::AssemblerDirective(directive) => match directive {
                    Directive::DataSection => {
                        // .data is only valid when in the .text section and if no .data section has
                        // previously been declared.
                        if self.section == Section::Text
                            && !self
                                .expressions
                                .contains(&Expression::Section(Directive::DataSection))
                        {
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
                        // .text is only valid when in the .data section and if no .text section has
                        // previously been declared.
                        if self.section == Section::Data
                            && !self
                                .expressions
                                .contains(&Expression::Section(Directive::TextSection))
                        {
                            self.section = Section::Text;
                            Ok(Expression::Section(directive))
                        } else {
                            Err(Error::DuplicateSectionDirective(codespan::Span::new(
                                self.lexer.span().start as u32,
                                self.lexer.span().end as u32,
                            )))
                        }
                    }
                    // .globl is valid anywhere.
                    Directive::GlobalSymbol => Directive::parse(directive, self),
                    _ => match self.section {
                        // Other directives are only valid in the .data section.
                        Section::Data => Directive::parse(directive, self),
                        Section::Text => Err(Error::UnexpectedToken {
                            got: Token::AssemblerDirective(directive),
                            span: self.lexer.span(),
                            expected: "instruction or label declaration",
                        }),
                    },
                },
                Token::Identifier(ident) => match self.section {
                    // Only label defs allowed in .data - instructions or independent labels are
                    // invalid.
                    Section::Data => Err(Error::UnexpectedToken {
                        got: Token::Identifier(ident),
                        span: self.lexer.span(),
                        expected: "assembler directive",
                    }),
                    Section::Text => match ident {
                        Identifier::Instruction(instr) => Instruction::parse(instr, self),
                        // Labels must be used with an instruction, cannot be independent.
                        label => Err(Error::UnexpectedToken {
                            got: Token::Identifier(label),
                            span: self.lexer.span(),
                            expected: "instruction or label definition",
                        }),
                    },
                },
                // Label defs are valid anywhere.
                Token::LabelDefinition(label) => {
                    self.defined_labels.push(label.clone());
                    Ok(Expression::LabelDefinition(label))
                }
                // Everything else cannot be the start of an expression.
                other => Err(match self.section {
                    Section::Data => Error::UnexpectedToken {
                        got: other,
                        span: self.lexer.span(),
                        expected: "assembler directive",
                    },
                    Section::Text => Error::UnexpectedToken {
                        got: other,
                        span: self.lexer.span(),
                        expected: "instruction or label definition",
                    },
                }),
            }?;
            self.expressions.push(expression);
        }
        Ok(())
    }

    fn second_pass(&mut self) -> Result<(), Error> {
        Ok(())
    }
}
