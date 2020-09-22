use crate::{expression::Expression, identifier::Identifier, parser::Error, token::{Token, parse_immediate}};
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

    pub fn parse(directive: Directive, lex: &mut Lexer<Token>) -> Result<Expression, Error> {
        match directive {
            Directive::DataSection | Directive::TextSection => Ok(Expression::Section(directive)),
            Directive::Align => Directive::parse_align(lex),
            Directive::AsciiString => Directive::parse_ascii(lex),
            Directive::AsciiStringNullTerminated => Directive::parse_asciiz(lex),
            Directive::ByteImmediate => Directive::parse_byte(lex),
            Directive::FloatImmediate => Directive::parse_float(lex),
            Directive::GlobalSymbol => Directive::parse_globl(lex),
            Directive::HalfWordImmediate => Directive::parse_half(lex),
            Directive::SpaceSection => Directive::parse_space(lex),
            Directive::WordImmediate => Directive::parse_word(lex),
        }
    }

    fn parse_align(lex: &mut Lexer<Token>) -> Result<Expression, Error> {
        let token = lex.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(ref string) => match parse_immediate(string) {
                Some(align) => {
                    if 0 <= align && align <= 3 {
                        Ok(Expression::IntegerDirective(Directive::ByteImmediate, align))
                    } else {
                        Err(Error::InvalidAlignment(token))
                    }
                },
                None => Err(Error::InvalidIntegerLiteral(token)),
            }
            Token::StringLiteral(_) => Err(Error::IncorrectLiteralType {
                got: token,
                expected: "integer literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                expected: "integer literal",
            }),
        }
    }

    fn parse_ascii(lex: &mut Lexer<Token>) -> Result<Expression, Error> {
        let token = lex.next().ok_or(Error::Eof)?;
        match token {
            Token::StringLiteral(string) => {
                Ok(Expression::StringDirective(Directive::AsciiString, string))
            }
            Token::NumberImmediate(_) => Err(Error::IncorrectLiteralType {
                got: token,
                expected: "string literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                expected: "string literal",
            }),
        }
    }

    fn parse_asciiz(lex: &mut Lexer<Token>) -> Result<Expression, Error> {
        let token = lex.next().ok_or(Error::Eof)?;
        match token {
            Token::StringLiteral(mut string) => {
                string.push('\0');
                Ok(Expression::StringDirective(Directive::AsciiString, string))
            }
            Token::NumberImmediate(_) => Err(Error::IncorrectLiteralType {
                got: token,
                expected: "string literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                expected: "string literal",
            }),
        }
    }

    fn parse_byte(lex: &mut Lexer<Token>) -> Result<Expression, Error> {
        let token = lex.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(string) => match parse_immediate(&string) {
                Some(int) => {
                    if int <= std::u8::MAX as i32 {
                        Ok(Expression::IntegerDirective(
                            Directive::HalfWordImmediate,
                            int
                        ))
                    } else {
                        Err(Error::ByteLiteralOutOfRange(Token::NumberImmediate(string)))
                    }
                },
                None => Err(Error::InvalidIntegerLiteral(Token::NumberImmediate(string))),
            }
            Token::NumberImmediateList(string) => {
                let mut bytes = Vec::new();
                for s in string.split(',').map(|s| s.trim()) {
                    match parse_immediate(s) {
                        Some(byte) => {
                            if byte < std::u8::MAX as i32 {
                                bytes.push(byte)
                            } else {
                                return Err(Error::ByteLiteralOutOfRange(
                                    Token::NumberImmediate(s.to_string())
                                ))
                            }
                        },
                        None => return Err(Error::InvalidFloatLiteral(Token::NumberImmediate(s.to_string())))
                    }
                }
                Ok(Expression::FloatsDirective(Directive::FloatImmediate, bytes))
            }
            Token::StringLiteral(_) => Err(Error::IncorrectLiteralType {
                got: token,
                expected: "half word literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                expected: "half word literal",
            }),
        }
    }

    fn parse_float(lex: &mut Lexer<Token>) -> Result<Expression, Error> {
        let token = lex.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(string) => match string.parse().ok() {
                Some(float) => Ok(Expression::FloatDirective(Directive::FloatImmediate, float)),
                None => Err(Error::InvalidFloatLiteral(Token::NumberImmediate(string))),
            }
            Token::NumberImmediateList(string) => {
                let mut floats = Vec::new();
                for s in string.split(',').map(|s| s.trim()) {
                    match s.parse().ok() {
                        Some(float) => floats.push(float),
                        None => return Err(Error::InvalidFloatLiteral(Token::NumberImmediate(s.to_string())))
                    }
                }
                Ok(Expression::IntegersDirective(Directive::ByteImmediate, bytes))
            }
            other => Err(Error::UnexpectedToken {
                got: other,
                expected: "float literal",
            })
        }
    }

    fn parse_globl(lex: &mut Lexer<Token>) -> Result<Expression, Error> {
        let token = lex.next().ok_or(Error::Eof)?;
        match token {
            Token::Identifier(Identifier::Label(label)) => {
                Ok(Expression::GlobalDirective(Directive::GlobalSymbol, label))
            }
            other => Err(Error::UnexpectedToken {
                got: other,
                expected: "label",
            }),
        }
    }

    fn parse_half(lex: &mut Lexer<Token>) -> Result<Expression, Error> {
        let token = lex.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(string) => match parse_immediate(&string) {
                Some(int) => {
                    if int <= std::u16::MAX as i32 {
                        Ok(Expression::IntegerDirective(
                            Directive::HalfWordImmediate,
                            int
                        ))
                    } else {
                        Err(Error::HalfLiteralOutOfRange(Token::NumberImmediate(string)))
                    }
                },
                None => Err(Error::InvalidIntegerLiteral(Token::NumberImmediate(string))),
            }
            Token::NumberImmediateList(string) => {
                let mut halves = Vec::new();
                for s in string.split(',').map(|s| s.trim()) {
                    match parse_immediate(s) {
                        Some(half) => {
                            if half < std::u16::MAX as i32 {
                                halves.push(half)
                            } else {
                                return Err(Error::ByteLiteralOutOfRange(
                                    Token::NumberImmediate(s.to_string())
                                ))
                            }
                        },
                        None => return Err(Error::InvalidFloatLiteral(Token::NumberImmediate(s.to_string())))
                    }
                }
                Ok(Expression::IntegersDirective(Directive::HalfWordImmediate, halvess))
            }
            Token::StringLiteral(_) => Err(Error::IncorrectLiteralType {
                got: token,
                expected: "half word literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                expected: "half word literal",
            }),
        }
    }

    fn parse_space(lex: &mut Lexer<Token>) -> Result<Expression, Error> {
        let token = lex.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(string) => match parse_immediate(&string) {
                Some(amt) => Ok(Expression::IntegerDirective(Directive::SpaceSection, amt)),
                None => Err(Error::InvalidIntegerLiteral(Token::NumberImmediate(string)))
            }
            Token::StringLiteral(_) => Err(Error::IncorrectLiteralType {
                got: token,
                expected: "integer literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                expected: "integer literal",
            }),
        }
    }

    fn parse_word(lex: &mut Lexer<Token>) -> Result<Expression, Error> {
        let token = lex.next().ok_or(Error::Eof)?;
        match token {
            Token::NumberImmediate(string) => match parse_immediate(&string) {
                Some(word) => Ok(Expression::IntegerDirective(Directive::WordImmediate, word)),
                None => Err(Error::InvalidIntegerLiteral(Token::NumberImmediate(string)))
            }
            Token::NumberImmediateList(string) => {
                let mut words = Vec::new();
                for s in string.split(',').map(|s| s.trim()) {
                    match parse_immediate(s) {
                        Some(word) => {
                            words.push(word)
                        },
                        None => return Err(Error::InvalidFloatLiteral(Token::NumberImmediate(s.to_string())))
                    }
                }
                Ok(Expression::IntegersDirective(Directive::WordImmediate, words))
            }
            Token::StringLiteral(_) => Err(Error::IncorrectLiteralType {
                got: token,
                expected: "word literal",
            }),
            other => Err(Error::UnexpectedToken {
                got: other,
                expected: "word literal",
            }),
        }
    }
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
