use crate::token::Token;
use logos::{Lexer, Logos};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Register {
    Zero,
    AssemblerTemporary,
    Values0,
    Values1,
    Arguments0,
    Arguments1,
    Arguments2,
    Arguments3,
    Temporary0,
    Temporary1,
    Temporary2,
    Temporary3,
    Temporary4,
    Temporary5,
    Temporary6,
    Temporary7,
    SavedTemporary0,
    SavedTemporary1,
    SavedTemporary2,
    SavedTemporary3,
    SavedTemporary4,
    SavedTemporary5,
    SavedTemporary6,
    SavedTemporary7,
    Temporary8,
    Temporary9,
    KernelReserved0,
    KernelReserved1,
    GlobalPointer,
    StackPointer,
    FramePointer,
    ReturnAddress,
    Coproc1F0,
    Coproc1F1,
    Coproc1F2,
    Coproc1F3,
    Coproc1F4,
    Coproc1F5,
    Coproc1F6,
    Coproc1F7,
    Coproc1F8,
    Coproc1F9,
    Coproc1F10,
    Coproc1F11,
    Coproc1F12,
    Coproc1F13,
    Coproc1F14,
    Coproc1F15,
    Coproc1F16,
    Coproc1F17,
    Coproc1F18,
    Coproc1F19,
    Coproc1F20,
    Coproc1F21,
    Coproc1F22,
    Coproc1F23,
    Coproc1F24,
    Coproc1F25,
    Coproc1F26,
    Coproc1F27,
    Coproc1F28,
    Coproc1F29,
    Coproc1F30,
    Coproc1F31,
}

impl Register {
    pub fn lex(lex: &mut Lexer<Token>) -> Option<Register> {
        Register::match_reg_str(lex.slice())
    }

    pub(crate) fn match_reg_str(s: &str) -> Option<Register> {
        match s {
            "$0" | "$zero" => Some(Register::Zero),
            "$1" | "$at" => Some(Register::AssemblerTemporary),
            "$2" | "$v0" => Some(Register::Values0),
            "$3" | "$v1" => Some(Register::Values1),
            "$4" | "$a0" => Some(Register::Arguments0),
            "$5" | "$a1" => Some(Register::Arguments1),
            "$6" | "$a2" => Some(Register::Arguments2),
            "$7" | "$a3" => Some(Register::Arguments3),
            "$8" | "$t0" => Some(Register::Temporary0),
            "$9" | "$t1" => Some(Register::Temporary1),
            "$10" | "$t2" => Some(Register::Temporary2),
            "$11" | "$t3" => Some(Register::Temporary3),
            "$12" | "$t4" => Some(Register::Temporary4),
            "$13" | "$t5" => Some(Register::Temporary5),
            "$14" | "$t6" => Some(Register::Temporary6),
            "$15" | "$t7" => Some(Register::Temporary7),
            "$16" | "$s0" => Some(Register::SavedTemporary0),
            "$17" | "$s1" => Some(Register::SavedTemporary1),
            "$18" | "$s2" => Some(Register::SavedTemporary2),
            "$19" | "$s3" => Some(Register::SavedTemporary3),
            "$20" | "$s4" => Some(Register::SavedTemporary4),
            "$21" | "$s5" => Some(Register::SavedTemporary5),
            "$22" | "$s6" => Some(Register::SavedTemporary6),
            "$23" | "$s7" => Some(Register::SavedTemporary7),
            "$24" | "$t8" => Some(Register::Temporary8),
            "$25" | "$t9" => Some(Register::Temporary9),
            "$26" | "$k0" => Some(Register::KernelReserved0),
            "$27" | "$k1" => Some(Register::KernelReserved1),
            "$28" | "$gp" => Some(Register::GlobalPointer),
            "$29" | "$sp" => Some(Register::StackPointer),
            "$30" | "$fp" => Some(Register::FramePointer),
            "$31" | "$ra" => Some(Register::ReturnAddress),
            "$f0" => Some(Register::Coproc1F0),
            "$f1" => Some(Register::Coproc1F1),
            "$f2" => Some(Register::Coproc1F2),
            "$f3" => Some(Register::Coproc1F3),
            "$f4" => Some(Register::Coproc1F4),
            "$f5" => Some(Register::Coproc1F5),
            "$f6" => Some(Register::Coproc1F6),
            "$f7" => Some(Register::Coproc1F7),
            "$f8" => Some(Register::Coproc1F8),
            "$f9" => Some(Register::Coproc1F9),
            "$f10" => Some(Register::Coproc1F10),
            "$f11" => Some(Register::Coproc1F11),
            "$f12" => Some(Register::Coproc1F12),
            "$f13" => Some(Register::Coproc1F13),
            "$f14" => Some(Register::Coproc1F14),
            "$f15" => Some(Register::Coproc1F15),
            "$f16" => Some(Register::Coproc1F16),
            "$f17" => Some(Register::Coproc1F17),
            "$f18" => Some(Register::Coproc1F18),
            "$f19" => Some(Register::Coproc1F19),
            "$f20" => Some(Register::Coproc1F20),
            "$f21" => Some(Register::Coproc1F21),
            "$f22" => Some(Register::Coproc1F22),
            "$f23" => Some(Register::Coproc1F23),
            "$f24" => Some(Register::Coproc1F24),
            "$f25" => Some(Register::Coproc1F25),
            "$f26" => Some(Register::Coproc1F26),
            "$f27" => Some(Register::Coproc1F27),
            "$f28" => Some(Register::Coproc1F28),
            "$f29" => Some(Register::Coproc1F29),
            "$f30" => Some(Register::Coproc1F30),
            "$f31" => Some(Register::Coproc1F31),
            _ => None,
        }
    }

    pub(crate) fn is_fp_reg(&self) -> bool {
        match self {
            Register::Coproc1F0
            | Register::Coproc1F1
            | Register::Coproc1F2
            | Register::Coproc1F3
            | Register::Coproc1F4
            | Register::Coproc1F5
            | Register::Coproc1F6
            | Register::Coproc1F7
            | Register::Coproc1F8
            | Register::Coproc1F9
            | Register::Coproc1F10
            | Register::Coproc1F11
            | Register::Coproc1F12
            | Register::Coproc1F13
            | Register::Coproc1F14
            | Register::Coproc1F15
            | Register::Coproc1F16
            | Register::Coproc1F17
            | Register::Coproc1F18
            | Register::Coproc1F19
            | Register::Coproc1F20
            | Register::Coproc1F21
            | Register::Coproc1F22
            | Register::Coproc1F23
            | Register::Coproc1F24
            | Register::Coproc1F25
            | Register::Coproc1F26
            | Register::Coproc1F27
            | Register::Coproc1F28
            | Register::Coproc1F29
            | Register::Coproc1F30
            | Register::Coproc1F31 => true,
            _ => false,
        }
    }
}

#[test]
fn test_lex_register() {
    let tests = [
        "$0",
        "$zero",
        "$1",
        "$at",
        "$2",
        "$v0",
        "$3",
        "$v1",
        "$4",
        "$a0",
        "$5",
        "$a1",
        "$6",
        "$a2",
        "$7",
        "$a3",
        "$8",
        "$t0",
        "$9",
        "$t1",
        "$10",
        "$t2",
        "$11",
        "$t3",
        "$12",
        "$t4",
        "$13",
        "$t5",
        "$14",
        "$t6",
        "$15",
        "$t7",
        "$16",
        "$s0",
        "$17",
        "$s1",
        "$18",
        "$s2",
        "$19",
        "$s3",
        "$20",
        "$s4",
        "$21",
        "$s5",
        "$22",
        "$s6",
        "$23",
        "$s7",
        "$24",
        "$t8",
        "$25",
        "$t9",
        "$26",
        "$k0",
        "$27",
        "$k1",
        "$28",
        "$gp",
        "$29",
        "$sp",
        "$30",
        "$fp",
        "$31",
        "$ra",
        "$f0",
        "$f1",
        "$f2",
        "$f3",
        "$f4",
        "$f5",
        "$f6",
        "$f7",
        "$f8",
        "$f9",
        "$f10",
        "$f11",
        "$f12",
        "$f13",
        "$f14",
        "$f15",
        "$f16",
        "$f17",
        "$f18",
        "$f19",
        "$f20",
        "$f21",
        "$f22",
        "$f23",
        "$f24",
        "$f25",
        "$f26",
        "$f27",
        "$f28",
        "$f29",
        "$f30",
        "$f31",
        "$notaregister",
    ];
    let results = [
        Token::Register(Register::Zero),
        Token::Register(Register::Zero),
        Token::Register(Register::AssemblerTemporary),
        Token::Register(Register::AssemblerTemporary),
        Token::Register(Register::Values0),
        Token::Register(Register::Values0),
        Token::Register(Register::Values1),
        Token::Register(Register::Values1),
        Token::Register(Register::Arguments0),
        Token::Register(Register::Arguments0),
        Token::Register(Register::Arguments1),
        Token::Register(Register::Arguments1),
        Token::Register(Register::Arguments2),
        Token::Register(Register::Arguments2),
        Token::Register(Register::Arguments3),
        Token::Register(Register::Arguments3),
        Token::Register(Register::Temporary0),
        Token::Register(Register::Temporary0),
        Token::Register(Register::Temporary1),
        Token::Register(Register::Temporary1),
        Token::Register(Register::Temporary2),
        Token::Register(Register::Temporary2),
        Token::Register(Register::Temporary3),
        Token::Register(Register::Temporary3),
        Token::Register(Register::Temporary4),
        Token::Register(Register::Temporary4),
        Token::Register(Register::Temporary5),
        Token::Register(Register::Temporary5),
        Token::Register(Register::Temporary6),
        Token::Register(Register::Temporary6),
        Token::Register(Register::Temporary7),
        Token::Register(Register::Temporary7),
        Token::Register(Register::SavedTemporary0),
        Token::Register(Register::SavedTemporary0),
        Token::Register(Register::SavedTemporary1),
        Token::Register(Register::SavedTemporary1),
        Token::Register(Register::SavedTemporary2),
        Token::Register(Register::SavedTemporary2),
        Token::Register(Register::SavedTemporary3),
        Token::Register(Register::SavedTemporary3),
        Token::Register(Register::SavedTemporary4),
        Token::Register(Register::SavedTemporary4),
        Token::Register(Register::SavedTemporary5),
        Token::Register(Register::SavedTemporary5),
        Token::Register(Register::SavedTemporary6),
        Token::Register(Register::SavedTemporary6),
        Token::Register(Register::SavedTemporary7),
        Token::Register(Register::SavedTemporary7),
        Token::Register(Register::Temporary8),
        Token::Register(Register::Temporary8),
        Token::Register(Register::Temporary9),
        Token::Register(Register::Temporary9),
        Token::Register(Register::KernelReserved0),
        Token::Register(Register::KernelReserved0),
        Token::Register(Register::KernelReserved1),
        Token::Register(Register::KernelReserved1),
        Token::Register(Register::GlobalPointer),
        Token::Register(Register::GlobalPointer),
        Token::Register(Register::StackPointer),
        Token::Register(Register::StackPointer),
        Token::Register(Register::FramePointer),
        Token::Register(Register::FramePointer),
        Token::Register(Register::ReturnAddress),
        Token::Register(Register::ReturnAddress),
        Token::Register(Register::Coproc1F0),
        Token::Register(Register::Coproc1F1),
        Token::Register(Register::Coproc1F2),
        Token::Register(Register::Coproc1F3),
        Token::Register(Register::Coproc1F4),
        Token::Register(Register::Coproc1F5),
        Token::Register(Register::Coproc1F6),
        Token::Register(Register::Coproc1F7),
        Token::Register(Register::Coproc1F8),
        Token::Register(Register::Coproc1F9),
        Token::Register(Register::Coproc1F10),
        Token::Register(Register::Coproc1F11),
        Token::Register(Register::Coproc1F12),
        Token::Register(Register::Coproc1F13),
        Token::Register(Register::Coproc1F14),
        Token::Register(Register::Coproc1F15),
        Token::Register(Register::Coproc1F16),
        Token::Register(Register::Coproc1F17),
        Token::Register(Register::Coproc1F18),
        Token::Register(Register::Coproc1F19),
        Token::Register(Register::Coproc1F20),
        Token::Register(Register::Coproc1F21),
        Token::Register(Register::Coproc1F22),
        Token::Register(Register::Coproc1F23),
        Token::Register(Register::Coproc1F24),
        Token::Register(Register::Coproc1F25),
        Token::Register(Register::Coproc1F26),
        Token::Register(Register::Coproc1F27),
        Token::Register(Register::Coproc1F28),
        Token::Register(Register::Coproc1F29),
        Token::Register(Register::Coproc1F30),
        Token::Register(Register::Coproc1F31),
        Token::Error,
    ];
    for (test, result) in tests.iter().copied().zip(results.iter().cloned()) {
        assert_eq!(Token::lexer(test).next().unwrap(), result);
    }
    let mut test = String::new();
    for s in tests.iter() {
        test.push_str(s);
        test.push('\n');
    }
    for (i, token) in Token::lexer(&test).enumerate() {
        assert_eq!(token, results[i]);
    }
}
