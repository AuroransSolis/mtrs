use crate::{
    directive::Directive, identifier::Identifier, instruction::Instruction, register::Register,
};
use logos::{Lexer, Logos};
use std::{
    fmt::{self, Debug},
    i32,
};

#[derive(Clone, Logos, PartialEq)]
pub enum Token {
    #[regex(r"\.[\w_]+", Directive::lex)]
    AssemblerDirective(Directive),
    #[token(",")]
    Comma,
    #[regex(r"0b[01]+|0o[0-7]+|0x[\da-fA-F]+|-?[\d]+(\.[\d]+)?", |lex| lex.slice().to_string())]
    NumberImmediate(String),
    #[regex(
        r"(0b[01]+|0o[0-7]+|0x[\da-fA-F]+|-?[\d]+(\.[\d]+)?)(, ?0b[01]+|0o[0-7]+|0x[\da-fA-F]+|-?[\d]+(\.[\d]+)?)+",
        |lex| lex.slice().to_string()
    )]
    NumberImmediateList(String),
    #[regex(r"[a-zA-Z_]+[a-zA-Z\d_\.]*", Identifier::lex)]
    Identifier(Identifier),
    #[regex(r"[\w_]+:", |lex| lex.slice().trim_end_matches(':').to_string())]
    LabelDefinition(String),
    #[regex(r"-?\d+\(\$[\w\d]+\)", lex_offset)]
    OffsetFrom((i32, Register)),
    #[regex(r"\$[\w\d]+", Register::lex)]
    Register(Register),
    #[regex(r##""[^"\r\n]+""##, |lex| lex.slice().to_string())]
    StringLiteral(String),
    #[regex(r"([ \t\n\f]+|#.+)", logos::skip)]
    #[error]
    Error,
}

impl Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::AssemblerDirective(directive) => write!(f, "{:?}", directive),
            Token::Comma => write!(f, "comma"),
            Token::NumberImmediate(immediate) => write!(f, "'{:?}'", immediate),
            Token::Identifier(identifier) => write!(f, "'{:?}'", identifier),
            Token::LabelDefinition(label_definition) => {
                write!(f, "label definition: '{}'", label_definition)
            }
            Token::OffsetFrom((offset, register)) => {
                write!(f, "offset {} from {:?}", offset, register)
            }
            Token::Register(register) => write!(f, "{:?}", register),
            Token::StringLiteral(string) => write!(f, "'{:?}'", string),
            Token::Error => write!(f, "error"),
        }
    }
}

pub(crate) fn parse_immediate(slice: &str) -> Option<i32> {
    if slice.starts_with("0b") {
        let slice = slice.trim_start_matches("0b");
        i32::from_str_radix(slice, 2).ok()
    } else if slice.starts_with("0o") {
        let slice = slice.trim_start_matches("0o");
        i32::from_str_radix(slice, 8).ok()
    } else if slice.starts_with("0x") {
        let slice = slice.trim_start_matches("0x");
        i32::from_str_radix(slice, 16).ok()
    } else {
        slice.parse().ok()
    }
}

fn lex_offset(lex: &Lexer<Token>) -> Option<(i32, Register)> {
    let slice = lex.slice();
    let mut spliterator = slice.split_terminator('(');
    let offset = spliterator.next().unwrap().parse().ok()?;
    let mut spliterator = spliterator.next().unwrap().split_terminator(')');
    Register::match_reg_str(spliterator.next().unwrap()).map(|reg| (offset, reg))
}

pub(crate) fn lspan_to_csspan(span: logos::Span) -> codespan::Span {
    codespan::Span::new(span.start as u32, span.end as u32)
}

#[test]
fn test_lex_comma() {
    let test = ",";
    let result = Token::Comma;
    assert_eq!(Token::lexer(test).next().unwrap(), result);
}

#[test]
fn test_lex_immediate() {
    let tests = [
        "0b1001011001101001",
        "0o0123443210",
        "0xaaaaaaa",
        "1337",
        "-420",
    ];
    let results = [
        Token::NumberImmediate("0b1001_0110_0110_1001".to_string()),
        Token::NumberImmediate("0o0123443210".to_string()),
        Token::NumberImmediate("0xaaaaaaa".to_string()),
        Token::NumberImmediate("1337".to_string()),
        Token::NumberImmediate("-420".to_string()),
    ];
    for (test, result) in tests.iter().copied().zip(results.iter().cloned()) {
        assert_eq!(Token::lexer(test).next().unwrap(), result);
    }
    let test = "0b1001011001101001 0o0123443210 0xaaaaaaa 1337 -420";
    for (i, token) in Token::lexer(test).enumerate() {
        assert_eq!(token, results[i]);
    }
}

#[test]
fn test_lex_label_definition() {
    let test = "main_function_label:";
    let result = Token::LabelDefinition("main_function_label".to_string());
    assert_eq!(Token::lexer(test).next().unwrap(), result);
}

#[test]
fn test_lex_offset_from() {
    let tests = [
        "0($0)", "1($zero)", "2($1)", "3($at)", "4($2)", "5($v0)", "6($3)", "7($v1)", "8($4)",
        "9($a0)", "10($5)", "11($a1)", "12($6)", "13($a2)", "14($7)", "15($a3)", "16($8)",
        "17($t0)", "18($9)", "19($t1)", "20($10)", "21($t2)", "22($11)", "23($t3)", "24($12)",
        "25($t4)", "26($13)", "27($t5)", "28($14)", "29($t6)", "30($15)", "31($t7)", "32($16)",
        "33($s0)", "34($17)", "35($s1)", "36($18)", "37($s2)", "38($19)", "39($s3)", "40($20)",
        "41($s4)", "42($21)", "43($s5)", "44($22)", "45($s6)", "46($23)", "47($s7)", "48($24)",
        "49($t8)", "50($25)", "51($t9)", "52($26)", "53($k0)", "54($27)", "55($k1)", "56($28)",
        "57($gp)", "58($29)", "59($sp)", "60($30)", "61($fp)", "62($31)", "63($ra)", "64($f0)",
        "65($f1)", "66($f2)", "67($f3)", "68($f4)", "69($f5)", "70($f6)", "71($f7)", "72($f8)",
        "73($f9)", "74($f10)", "75($f11)", "76($f12)", "77($f13)", "78($f14)", "79($f15)",
        "80($f16)", "81($f17)", "82($f18)", "83($f19)", "84($f20)", "85($f21)", "86($f22)",
        "87($f23)", "88($f24)", "89($f25)", "90($f26)", "91($f27)", "92($f28)", "93($f29)",
        "94($f30)", "95($f31)",
    ];
    let results = [
        Register::Zero,
        Register::Zero,
        Register::AssemblerTemporary,
        Register::AssemblerTemporary,
        Register::Values0,
        Register::Values0,
        Register::Values1,
        Register::Values1,
        Register::Arguments0,
        Register::Arguments0,
        Register::Arguments1,
        Register::Arguments1,
        Register::Arguments2,
        Register::Arguments2,
        Register::Arguments3,
        Register::Arguments3,
        Register::Temporary0,
        Register::Temporary0,
        Register::Temporary1,
        Register::Temporary1,
        Register::Temporary2,
        Register::Temporary2,
        Register::Temporary3,
        Register::Temporary3,
        Register::Temporary4,
        Register::Temporary4,
        Register::Temporary5,
        Register::Temporary5,
        Register::Temporary6,
        Register::Temporary6,
        Register::Temporary7,
        Register::Temporary7,
        Register::SavedTemporary0,
        Register::SavedTemporary0,
        Register::SavedTemporary1,
        Register::SavedTemporary1,
        Register::SavedTemporary2,
        Register::SavedTemporary2,
        Register::SavedTemporary3,
        Register::SavedTemporary3,
        Register::SavedTemporary4,
        Register::SavedTemporary4,
        Register::SavedTemporary5,
        Register::SavedTemporary5,
        Register::SavedTemporary6,
        Register::SavedTemporary6,
        Register::SavedTemporary7,
        Register::SavedTemporary7,
        Register::Temporary8,
        Register::Temporary8,
        Register::Temporary9,
        Register::Temporary9,
        Register::KernelReserved0,
        Register::KernelReserved0,
        Register::KernelReserved1,
        Register::KernelReserved1,
        Register::GlobalPointer,
        Register::GlobalPointer,
        Register::StackPointer,
        Register::StackPointer,
        Register::FramePointer,
        Register::FramePointer,
        Register::ReturnAddress,
        Register::ReturnAddress,
        Register::Coproc1F0,
        Register::Coproc1F1,
        Register::Coproc1F2,
        Register::Coproc1F3,
        Register::Coproc1F4,
        Register::Coproc1F5,
        Register::Coproc1F6,
        Register::Coproc1F7,
        Register::Coproc1F8,
        Register::Coproc1F9,
        Register::Coproc1F10,
        Register::Coproc1F11,
        Register::Coproc1F12,
        Register::Coproc1F13,
        Register::Coproc1F14,
        Register::Coproc1F15,
        Register::Coproc1F16,
        Register::Coproc1F17,
        Register::Coproc1F18,
        Register::Coproc1F19,
        Register::Coproc1F20,
        Register::Coproc1F21,
        Register::Coproc1F22,
        Register::Coproc1F23,
        Register::Coproc1F24,
        Register::Coproc1F25,
        Register::Coproc1F26,
        Register::Coproc1F27,
        Register::Coproc1F28,
        Register::Coproc1F29,
        Register::Coproc1F30,
        Register::Coproc1F31,
    ];
    for (i, (test, result)) in tests
        .iter()
        .copied()
        .zip(results.iter().cloned())
        .enumerate()
    {
        assert_eq!(
            Token::lexer(test).next().unwrap(),
            Token::OffsetFrom((i as i32, result))
        );
    }
    let mut test = String::new();
    for s in tests.iter() {
        test.push_str(&format!("{}\n", s));
    }
    for (i, token) in Token::lexer(&test).enumerate() {
        assert_eq!(token, Token::OffsetFrom((i as i32, results[i])));
    }
}

#[test]
fn test_lex_string_literal() {
    let test = "\"Hello world!\"";
    let result = Token::StringLiteral("\"Hello world!\"".to_string());
    assert_eq!(Token::lexer(test).next().unwrap(), result);
}
