use crate::Token;
use logos::Lexer;

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
}

impl Register {
    pub fn lex(lex: &mut Lexer<Token>) -> Option<Register> {
        match lex.slice() {
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
            _ => None,
        }
    }
}