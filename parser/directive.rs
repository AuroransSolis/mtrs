use crate::{
    expression::Expression,
    identifier::Identifier,
    parser::{Error, Parser},
    token::{parse_immediate, Token},
};
use logos::{Lexer, Logos};
use std::f32;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Directive {
    Align,
    AsciiString,
    AsciiStringNullTerminated,
    ByteImmediate,
    DataSection,
    FloatImmediate,
    GlobalSymbol,
    HalfWordImmediate,
    SpaceSection,
    TextSection,
    WordImmediate,
}

impl Directive {
    pub fn lex(lex: &mut Lexer<Token>) -> Option<Directive> {
        match lex.slice() {
            ".align" => Some(Directive::Align),
            ".ascii" => Some(Directive::AsciiString),
            ".asciiz" => Some(Directive::AsciiStringNullTerminated),
            ".byte" => Some(Directive::ByteImmediate),
            ".data" => Some(Directive::DataSection),
            ".float" => Some(Directive::FloatImmediate),
            ".globl" => Some(Directive::GlobalSymbol),
            ".half" => Some(Directive::HalfWordImmediate),
            ".space" => Some(Directive::SpaceSection),
            ".text" => Some(Directive::TextSection),
            ".word" => Some(Directive::WordImmediate),
            _ => None,
        }
    }

    pub fn parse(directive: Directive, parser: &mut Parser) -> Result<Expression, Error> {
        match directive {
            Directive::DataSection | Directive::TextSection => Ok(Expression::Section(directive)),
            Directive::Align => Directive::parse_align(parser),
            Directive::AsciiString => Directive::parse_ascii(parser),
            Directive::AsciiStringNullTerminated => Directive::parse_asciiz(parser),
            Directive::ByteImmediate => Directive::parse_byte(parser),
            Directive::FloatImmediate => Directive::parse_float(parser),
            Directive::GlobalSymbol => Directive::parse_globl(parser),
            Directive::HalfWordImmediate => Directive::parse_half(parser),
            Directive::SpaceSection => Directive::parse_space(parser),
            Directive::WordImmediate => Directive::parse_word(parser),
        }
    }

    fn parse_align(parser: &mut Parser) -> Result<Expression, Error> {
        let token = parser.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(ref string) => match parse_immediate(string) {
                Some(align) => {
                    if 0 <= align && align <= 3 {
                        Ok(Expression::IntegerDirective(
                            Directive::ByteImmediate,
                            align,
                        ))
                    } else {
                        Err(Error::InvalidAlignment {
                            token,
                            span: parser.lexer.span(),
                        })
                    }
                }
                None => Err(Error::InvalidIntegerLiteral(token, parser.lexer.span())),
            },
            Token::StringLiteral(_) => Err(Error::IncorrectLiteralType {
                got: token,
                span: parser.lexer.span(),
                expected: "integer literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "integer literal",
            }),
        }
    }

    fn parse_ascii(parser: &mut Parser) -> Result<Expression, Error> {
        let token = parser.next().ok_or(Error::Eof)?;
        match token {
            Token::StringLiteral(string) => {
                Ok(Expression::StringDirective(Directive::AsciiString, string))
            }
            Token::NumberImmediate(_) => Err(Error::IncorrectLiteralType {
                got: token,
                span: parser.lexer.span(),
                expected: "string literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "string literal",
            }),
        }
    }

    fn parse_asciiz(parser: &mut Parser) -> Result<Expression, Error> {
        let token = parser.next().ok_or(Error::Eof)?;
        match token {
            Token::StringLiteral(mut string) => {
                string.push('\0');
                Ok(Expression::StringDirective(Directive::AsciiString, string))
            }
            Token::NumberImmediate(_) => Err(Error::IncorrectLiteralType {
                got: token,
                span: parser.lexer.span(),
                expected: "string literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "string literal",
            }),
        }
    }

    fn parse_byte(parser: &mut Parser) -> Result<Expression, Error> {
        let token = parser.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(string) => match parse_immediate(&string) {
                Some(int) => {
                    if int <= std::u8::MAX as i32 {
                        match parser.next().ok_or(Error::Eof)? {
                            Token::Comma => Ok(Expression::IntegersDirective(
                                Directive::ByteImmediate,
                                get_bytes_list(parser)?,
                            )),
                            other => {
                                parser.carry = Some(other);
                                Ok(Expression::IntegerDirective(Directive::ByteImmediate, int))
                            }
                        }
                    } else {
                        Err(Error::ByteLiteralOutOfRange {
                            token: Token::NumberImmediate(string),
                            span: parser.lexer.span(),
                        })
                    }
                }
                None => Err(Error::InvalidIntegerLiteral(
                    Token::NumberImmediate(string),
                    parser.lexer.span(),
                )),
            },
            Token::StringLiteral(_) => Err(Error::IncorrectLiteralType {
                got: token,
                span: parser.lexer.span(),
                expected: "half word literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "half word literal",
            }),
        }
    }

    fn parse_float(parser: &mut Parser) -> Result<Expression, Error> {
        let token = parser.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(string) => match string.parse().ok() {
                Some(float) => match parser.next().ok_or(Error::Eof)? {
                    Token::Comma => Ok(Expression::FloatsDirective(
                        Directive::FloatImmediate,
                        get_floats_list(parser)?,
                    )),
                    other => {
                        parser.carry = Some(other);
                        Ok(Expression::FloatDirective(Directive::FloatImmediate, float))
                    }
                },
                None => Err(Error::InvalidFloatLiteral(
                    Token::NumberImmediate(string),
                    parser.lexer.span(),
                )),
            },
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "float literal",
            }),
        }
    }

    fn parse_globl(parser: &mut Parser) -> Result<Expression, Error> {
        let token = parser.next().ok_or(Error::Eof)?;
        match token {
            Token::Identifier(Identifier::Label(label)) => {
                Ok(Expression::GlobalDirective(Directive::GlobalSymbol, label))
            }
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "label",
            }),
        }
    }

    fn parse_half(parser: &mut Parser) -> Result<Expression, Error> {
        let token = parser.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(string) => match parse_immediate(&string) {
                Some(int) => {
                    if int <= std::u16::MAX as i32 {
                        match parser.next().ok_or(Error::Eof)? {
                            Token::Comma => Ok(Expression::IntegersDirective(
                                Directive::HalfWordImmediate,
                                get_halves_list(parser)?,
                            )),
                            other => {
                                parser.carry = Some(other);
                                Ok(Expression::IntegerDirective(
                                    Directive::HalfWordImmediate,
                                    int,
                                ))
                            }
                        }
                    } else {
                        Err(Error::HalfLiteralOutOfRange {
                            token: Token::NumberImmediate(string),
                            span: parser.lexer.span(),
                        })
                    }
                }
                None => Err(Error::InvalidIntegerLiteral(
                    Token::NumberImmediate(string),
                    parser.lexer.span(),
                )),
            },
            Token::StringLiteral(_) => Err(Error::IncorrectLiteralType {
                got: token,
                span: parser.lexer.span(),
                expected: "half word literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "half word literal",
            }),
        }
    }

    fn parse_space(parser: &mut Parser) -> Result<Expression, Error> {
        let token = parser.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(string) => match parse_immediate(&string) {
                Some(amt) => Ok(Expression::IntegerDirective(Directive::SpaceSection, amt)),
                None => Err(Error::InvalidIntegerLiteral(
                    Token::NumberImmediate(string),
                    parser.lexer.span(),
                )),
            },
            Token::StringLiteral(_) => Err(Error::IncorrectLiteralType {
                got: token,
                span: parser.lexer.span(),
                expected: "integer literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "integer literal",
            }),
        }
    }

    fn parse_word(parser: &mut Parser) -> Result<Expression, Error> {
        let token = parser.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(string) => match parse_immediate(&string) {
                Some(word) => match parser.next().ok_or(Error::Eof)? {
                    Token::Comma => Ok(Expression::IntegersDirective(
                        Directive::WordImmediate,
                        get_words_list(parser)?,
                    )),
                    other => {
                        parser.carry = Some(other);
                        Ok(Expression::IntegerDirective(Directive::WordImmediate, word))
                    }
                },
                None => Err(Error::InvalidIntegerLiteral(
                    Token::NumberImmediate(string),
                    parser.lexer.span(),
                )),
            },
            Token::StringLiteral(_) => Err(Error::IncorrectLiteralType {
                got: token,
                span: parser.lexer.span(),
                expected: "word literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "word literal",
            }),
        }
    }
}

fn get_bytes_list(parser: &mut Parser) -> Result<Vec<i32>, Error> {
    let mut bytes = Vec::new();
    loop {
        let token = parser.next().ok_or(Error::Eof)?;
        bytes.push(match token {
            Token::NumberImmediate(ref string) => match parse_immediate(string) {
                Some(int) => {
                    if int < std::u8::MAX as i32 {
                        Ok(int)
                    } else {
                        Err(Error::ByteLiteralOutOfRange {
                            token,
                            span: parser.lexer.span(),
                        })
                    }
                }
                None => Err(Error::InvalidIntegerLiteral(token, parser.lexer.span())),
            },
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "integer literal",
            }),
        }?);
        match parser.next().ok_or(Error::Eof)? {
            Token::Comma => continue,
            other => {
                parser.carry = Some(other);
                break;
            }
        }
    }
    Ok(bytes)
}

fn get_floats_list(parser: &mut Parser) -> Result<Vec<f32>, Error> {
    let mut floats = Vec::new();
    loop {
        let token = parser.next().ok_or(Error::Eof)?;
        floats.push(match token {
            Token::NumberImmediate(ref string) => match string.parse().ok() {
                Some(float) => Ok(float),
                None => Err(Error::InvalidFloatLiteral(token, parser.lexer.span())),
            },
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "integer literal",
            }),
        }?);
        match parser.next().ok_or(Error::Eof)? {
            Token::Comma => continue,
            other => {
                parser.carry = Some(other);
                break;
            }
        }
    }
    Ok(floats)
}

fn get_halves_list(parser: &mut Parser) -> Result<Vec<i32>, Error> {
    let mut halves = Vec::new();
    loop {
        let token = parser.next().ok_or(Error::Eof)?;
        halves.push(match token {
            Token::NumberImmediate(ref string) => match parse_immediate(string) {
                Some(int) => {
                    if int < std::u16::MAX as i32 {
                        Ok(int)
                    } else {
                        Err(Error::HalfLiteralOutOfRange {
                            token,
                            span: parser.lexer.span(),
                        })
                    }
                }
                None => Err(Error::InvalidIntegerLiteral(token, parser.lexer.span())),
            },
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "integer literal",
            }),
        }?);
        match parser.next().ok_or(Error::Eof)? {
            Token::Comma => continue,
            other => {
                parser.carry = Some(other);
                break;
            }
        }
    }
    Ok(halves)
}

fn get_words_list(parser: &mut Parser) -> Result<Vec<i32>, Error> {
    let mut words = Vec::new();
    loop {
        let token = parser.next().ok_or(Error::Eof)?;
        words.push(match token {
            Token::NumberImmediate(ref string) => parse_immediate(string)
                .ok_or(Error::InvalidIntegerLiteral(token, parser.lexer.span())),
            other => Err(Error::UnexpectedToken {
                got: other,
                span: parser.lexer.span(),
                expected: "integer literal",
            }),
        }?);
        match parser.next().ok_or(Error::Eof)? {
            Token::Comma => continue,
            other => {
                parser.carry = Some(other);
                break;
            }
        }
    }
    Ok(words)
}

#[test]
fn test_lex_assembler_directive() {
    let tests = [
        ".align",
        ".ascii",
        ".asciiz",
        ".byte",
        ".data",
        ".globl",
        ".half",
        ".space",
        ".text",
        ".word",
        ".notadirective",
    ];
    let results = [
        Token::AssemblerDirective(Directive::Align),
        Token::AssemblerDirective(Directive::AsciiString),
        Token::AssemblerDirective(Directive::AsciiStringNullTerminated),
        Token::AssemblerDirective(Directive::ByteImmediate),
        Token::AssemblerDirective(Directive::DataSection),
        Token::AssemblerDirective(Directive::GlobalSymbol),
        Token::AssemblerDirective(Directive::HalfWordImmediate),
        Token::AssemblerDirective(Directive::SpaceSection),
        Token::AssemblerDirective(Directive::TextSection),
        Token::AssemblerDirective(Directive::WordImmediate),
        Token::Error,
    ];
    for (test, result) in tests.iter().copied().zip(results.iter().cloned()) {
        assert_eq!(Token::lexer(test).next().unwrap(), result);
    }
    let test = r#".align
    .ascii
    .asciiz
    .byte
    .data
    .globl
    .half
    .space
    .text
    .word
    .notadirective"#;
    for (i, token) in Token::lexer(test).enumerate() {
        assert_eq!(token, results[i]);
    }
}
