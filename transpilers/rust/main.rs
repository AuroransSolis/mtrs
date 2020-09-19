use logos::Logos;
use mips_parser::Token;

const TEST: &str = r#".data
.globl main
.text
 
main:
	addi $s0, $zero, 20 # Initialize $s0 to 20
	addi $s1, $zero, 30 # Initialize $s1 to 30
	addi $s2, $zero, -2 # Initialize $s2 to -2"#;
    
fn main() {
    let mut lex = Token::lexer(TEST);
    while let Some(token) = lex.next() {
        if token != Token::Error {
            println!("token: {:?}", token);
        }
    }
}