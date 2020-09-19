use crate::Token;
use logos::Lexer;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Immediate {
    IntegerLiteral(i32),
    SingleLiteral(f32),
}

impl Immediate {
    pub fn lex(lex: &mut Lexer<Token>) -> Option<Immediate> {
        let slice = &lex.slice()[..];
        slice
            .parse()
            .ok()
            .map(|word| Immediate::IntegerLiteral(word))
            .or_else(|| {
                slice
                    .parse()
                    .ok()
                    .map(|single| Immediate::SingleLiteral(single))
            })
    }
}