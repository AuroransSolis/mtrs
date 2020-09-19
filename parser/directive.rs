use crate::Token;
use logos::Lexer;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Directive {
    Align,
    AsciiString,
    AsciiStringNullTerminated,
    ByteImmediate,
    DataSection,
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
            ".globl" => Some(Directive::GlobalSymbol),
            ".half" => Some(Directive::HalfWordImmediate),
            ".space" => Some(Directive::SpaceSection),
            ".text" => Some(Directive::TextSection),
            ".word" => Some(Directive::WordImmediate),
            _ => None
        }
    }
}