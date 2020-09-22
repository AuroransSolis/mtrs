use crate::{directive::Directive, instruction::Instruction, register::Register};

#[derive(Clone, Debug)]
pub enum Expression {
    Section(Directive),
    FloatDirective(Directive, f32),
    FloatsDirective(Directive, Vec<f32>),
    IntegerDirective(Directive, i32),
    IntegersDirective(Directive, Vec<i32>),
    StringDirective(Directive, String),
    GlobalDirective(Directive, String),
    LabelDefinition(String),
    Cp03(Instruction, Register, Register, Register),
    Cp02(Instruction, Register, Register),
    Cp02Imm1(Instruction, Register, Register, i32),
    Cp02Lab1(Instruction, Register, Register, String),
    Cp01(Instruction, Register),
    Cp01Cp11(Instruction, Register, Register),
    Cp01Lab1(Instruction, Register, String),
    Cp01Off1(Instruction, Register, i32, Register),
    Lab1(Instruction, String),
    Cp13(Instruction, Register, Register, Register),
    Cp12(Instruction, Register, Register),
    Cp11Off1(Instruction, Register, i32, Register),
    NoArgs(Instruction),
}
