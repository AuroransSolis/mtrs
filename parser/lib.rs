mod directive;
mod immediate;
mod instruction;
mod register;

use directive::Directive;
use immediate::Immediate;
use instruction::Instruction;
use logos::Logos;
use register::Register;

#[derive(Clone, Debug, Logos, PartialEq)]
pub enum Token {
    #[regex(r"\.(align|ascii[z]?|byte|data|globl|half|space|text|word)", Directive::lex)]
    AssemblerDirective(Directive),
    #[regex(r"(0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+|-?[0-9]+)", Immediate::lex)]
    Immediate(Immediate),
    #[regex(r"this is gonna be long as shit lmao", Instruction::lex)]
    Instruction(Instruction),
    #[regex(r"[\w_]+:", |lex| lex.slice().trim_end_matches(':').to_string())]
    Label(String),
    #[regex(
        r"\$(\d|\d\d|zero|at|v[0-1]|a[0-3]|t[0-9]|s[0-7]|k[0-1]|[gsf]p|ra|f[0-31])",
        Register::lex
    )]
    Register(Register),
    #[regex(r"([ \t\n\f]+|#.+)", logos::skip)]
    #[error]
    Error,
}