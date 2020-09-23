use crate::{
    expression::Expression,
    identifier::Identifier,
    parser::{Error, Parser},
    register::Register,
    token::{lspan_to_csspan, parse_immediate, Token},
};
use logos::Lexer;

// This is probably incomplete - I've pieced this together from several websites and PDFs, and I'm
// not even sure what MIPS revision(s) any of them were using.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Instruction {
    Add,
    AddImmediate,
    AddImmediateUnsigned,
    AddUnsigned,
    And,
    AndImmediate,
    BranchOnEq,
    BranchOnGt,
    BranchOnGte,
    BranchOnGteZero,
    BranchOnGteZeroAndLink,
    BranchOnGtZero,
    BranchOnLt,
    BranchOnLte,
    BranchOnLteZero,
    BranchOnLtZero,
    BranchOnLtZeroAndLink,
    BranchOnNotEq,
    Divide,
    DivideUnsigned,
    FpAbsoluteValueDouble,
    FpAbsoluteValueSingle,
    FpAdditionDouble,
    FpAdditionSingle,
    FpBranchOnConditionFalse,
    FpBranchOnConditionTrue,
    FpCeilingDoubleToInteger,
    FpCeilingSingleToInteger,
    FpCompareEqDouble,
    FpCompareEqSingle,
    FpCompareGtDouble,
    FpCompareGtSingle,
    FpCompareGteDouble,
    FpCompareGteSingle,
    FpCompareLtDouble,
    FpCompareLtSingle,
    FpCompareLteDouble,
    FpCompareLteSingle,
    FpConditionalMoveCp0,
    FpConditionalMoveDoubleCp1,
    FpConditionalMoveSingleCp1,
    FpConvertSingleToDouble,
    FpConvertIntegerToDouble,
    FpConvertDoubleToSingle,
    FpConvertIntegerToSingle,
    FpConvertDoubleToInteger,
    FpConvertSingleToInteger,
    FpDivideDouble,
    FpDivideSingle,
    FpLoadDouble,
    FpLoadSingle,
    FpMoveDouble,
    FpMoveSingle,
    FpMultiplyDouble,
    FpMultiplySingle,
    FpNegateDouble,
    FpNegateSingle,
    FpSquareRootDouble,
    FpSquareRootSingle,
    FpStoreDouble,
    FpStoreSingle,
    FpSubtractDouble,
    FpSubtractSingle,
    Jump,
    JumpAndLink,
    JumpRegister,
    LoadAddress,
    LoadByte,
    LoadByteUnsigned,
    LoadHalfword,
    LoadHalfwordUnsigned,
    LoadImmediate,
    LoadUpperImmediate,
    LoadWord,
    Move,
    MoveFromC1,
    MoveFromHi,
    MoveFromLo,
    MoveToC1,
    Multiply,
    MultiplyUnsigned,
    MultiplyWithDest,
    NoOp,
    Or,
    OrImmediate,
    SetOnGt,
    SetOnGtImmediate,
    SetOnGtImmediateUnsigned,
    SetOnGtUnsigned,
    SetOnLt,
    SetOnLtImmediate,
    SetOnLtImmediateUnsigned,
    SetOnLtUnsigned,
    ShiftLeftLogical,
    ShiftLeftLogicalVariable,
    ShiftRightArithmetic,
    ShiftRightArithmeticVariable,
    ShiftRightLogical,
    ShiftRightLogicalVariable,
    StoreByte,
    StoreConditional,
    StoreHalfword,
    StoreWord,
    Subtract,
    SubtractUnsigned,
    Syscall,
    Xor,
    XorImmediate,
}

impl Instruction {
    pub fn lex(lex: &mut Lexer<Token>) -> Option<Instruction> {
        match lex.slice() {
            "add" => Some(Instruction::Add),
            "addi" => Some(Instruction::AddImmediate),
            "addiu" => Some(Instruction::AddImmediateUnsigned),
            "addu" => Some(Instruction::AddUnsigned),
            "and" => Some(Instruction::And),
            "andi" => Some(Instruction::AndImmediate),
            "beq" => Some(Instruction::BranchOnEq),
            "bgt" => Some(Instruction::BranchOnGt),
            "bge" => Some(Instruction::BranchOnGte),
            "bgez" => Some(Instruction::BranchOnGteZero),
            "bgezal" => Some(Instruction::BranchOnGteZeroAndLink),
            "bgtz" => Some(Instruction::BranchOnGtZero),
            "blt" => Some(Instruction::BranchOnLt),
            "ble" => Some(Instruction::BranchOnLte),
            "blez" => Some(Instruction::BranchOnLteZero),
            "bltz" => Some(Instruction::BranchOnLtZero),
            "bltzal" => Some(Instruction::BranchOnLtZeroAndLink),
            "bne" => Some(Instruction::BranchOnNotEq),
            "div" => Some(Instruction::Divide),
            "divu" => Some(Instruction::DivideUnsigned),
            "abs.d" => Some(Instruction::FpAbsoluteValueDouble),
            "abs.s" => Some(Instruction::FpAbsoluteValueSingle),
            "add.d" => Some(Instruction::FpAdditionDouble),
            "add.s" => Some(Instruction::FpAdditionSingle),
            "bc1f" => Some(Instruction::FpBranchOnConditionFalse),
            "bc1t" => Some(Instruction::FpBranchOnConditionTrue),
            "ceil.w.d" => Some(Instruction::FpCeilingDoubleToInteger),
            "ceil.w.s" => Some(Instruction::FpCeilingSingleToInteger),
            "c.eq.d" => Some(Instruction::FpCompareEqDouble),
            "c.eq.s" => Some(Instruction::FpCompareEqSingle),
            "c.gt.d" => Some(Instruction::FpCompareGtDouble),
            "c.gt.s" => Some(Instruction::FpCompareGtSingle),
            "c.ge.d" => Some(Instruction::FpCompareGteDouble),
            "c.ge.s" => Some(Instruction::FpCompareGteSingle),
            "c.lt.d" => Some(Instruction::FpCompareLtDouble),
            "c.lt.s" => Some(Instruction::FpCompareLtSingle),
            "c.le.d" => Some(Instruction::FpCompareLteDouble),
            "c.le.s" => Some(Instruction::FpCompareLteSingle),
            "movf" => Some(Instruction::FpConditionalMoveCp0),
            "movt.d" => Some(Instruction::FpConditionalMoveDoubleCp1),
            "movt.s" => Some(Instruction::FpConditionalMoveSingleCp1),
            "cvt.d.s" => Some(Instruction::FpConvertSingleToDouble),
            "cvt.d.w" => Some(Instruction::FpConvertIntegerToDouble),
            "cvt.s.d" => Some(Instruction::FpConvertDoubleToSingle),
            "cvt.s.w" => Some(Instruction::FpConvertIntegerToSingle),
            "cvt.w.d" => Some(Instruction::FpConvertDoubleToInteger),
            "cvt.w.s" => Some(Instruction::FpConvertSingleToInteger),
            "div.d" => Some(Instruction::FpDivideDouble),
            "div.s" => Some(Instruction::FpDivideSingle),
            "l.d" | "ldc1" => Some(Instruction::FpLoadDouble),
            "l.s" | "lwc1" => Some(Instruction::FpLoadSingle),
            "mov.d" => Some(Instruction::FpMoveDouble),
            "mov.s" => Some(Instruction::FpMoveSingle),
            "mul.d" => Some(Instruction::FpMultiplyDouble),
            "mul.s" => Some(Instruction::FpMultiplySingle),
            "neg.d" => Some(Instruction::FpNegateDouble),
            "neg.s" => Some(Instruction::FpNegateSingle),
            "sqrt.d" => Some(Instruction::FpSquareRootDouble),
            "sqrt.s" => Some(Instruction::FpSquareRootSingle),
            "s.d" | "sdc1" => Some(Instruction::FpStoreDouble),
            "s.s" | "swc1" => Some(Instruction::FpStoreSingle),
            "sub.d" => Some(Instruction::FpSubtractDouble),
            "sub.s" => Some(Instruction::FpSubtractSingle),
            "j" => Some(Instruction::Jump),
            "jal" => Some(Instruction::JumpAndLink),
            "jr" => Some(Instruction::JumpRegister),
            "la" => Some(Instruction::LoadAddress),
            "lb" => Some(Instruction::LoadByte),
            "lbu" => Some(Instruction::LoadByteUnsigned),
            "lh" => Some(Instruction::LoadHalfword),
            "lhu" => Some(Instruction::LoadHalfwordUnsigned),
            "li" => Some(Instruction::LoadImmediate),
            "lui" => Some(Instruction::LoadUpperImmediate),
            "lw" => Some(Instruction::LoadWord),
            "move" => Some(Instruction::Move),
            "mfc1" => Some(Instruction::MoveFromC1),
            "mfhi" => Some(Instruction::MoveFromHi),
            "mflo" => Some(Instruction::MoveFromLo),
            "mtc1" => Some(Instruction::MoveToC1),
            "mult" => Some(Instruction::Multiply),
            "multu" => Some(Instruction::MultiplyUnsigned),
            "mul" => Some(Instruction::MultiplyWithDest),
            "noop" => Some(Instruction::NoOp),
            "or" => Some(Instruction::Or),
            "ori" => Some(Instruction::OrImmediate),
            "sb" => Some(Instruction::StoreByte),
            "sc" => Some(Instruction::StoreConditional),
            "sgt" => Some(Instruction::SetOnGt),
            "sgti" => Some(Instruction::SetOnGtImmediate),
            "sgtiu" => Some(Instruction::SetOnGtImmediateUnsigned),
            "sgtu" => Some(Instruction::SetOnGtUnsigned),
            "sh" => Some(Instruction::StoreHalfword),
            "sll" => Some(Instruction::ShiftLeftLogical),
            "sllv" => Some(Instruction::ShiftLeftLogicalVariable),
            "slt" => Some(Instruction::SetOnLt),
            "slti" => Some(Instruction::SetOnLtImmediate),
            "sltiu" => Some(Instruction::SetOnLtImmediateUnsigned),
            "sltu" => Some(Instruction::SetOnLtUnsigned),
            "sra" => Some(Instruction::ShiftRightArithmetic),
            "srav" => Some(Instruction::ShiftRightArithmeticVariable),
            "srl" => Some(Instruction::ShiftRightLogical),
            "srlv" => Some(Instruction::ShiftRightLogicalVariable),
            "sub" => Some(Instruction::Subtract),
            "subu" => Some(Instruction::SubtractUnsigned),
            "sw" => Some(Instruction::StoreWord),
            "syscall" => Some(Instruction::Syscall),
            "xor" => Some(Instruction::Xor),
            "xori" => Some(Instruction::XorImmediate),
            _ => None,
        }
    }

    pub fn parse(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        match instruction {
            // 3 CP0 regs
            Instruction::Add
            | Instruction::AddUnsigned
            | Instruction::And
            | Instruction::Divide
            | Instruction::DivideUnsigned
            | Instruction::Multiply
            | Instruction::MultiplyUnsigned
            | Instruction::MultiplyWithDest
            | Instruction::Or
            | Instruction::ShiftLeftLogicalVariable
            | Instruction::ShiftRightArithmeticVariable
            | Instruction::ShiftRightLogicalVariable
            | Instruction::SetOnGt
            | Instruction::SetOnGtUnsigned
            | Instruction::SetOnLt
            | Instruction::SetOnLtUnsigned
            | Instruction::Subtract
            | Instruction::SubtractUnsigned
            | Instruction::Xor => Instruction::parse_3cp0(instruction, parser),
            // 2 CP0 regs
            Instruction::Move | Instruction::FpConditionalMoveCp0 => {
                Instruction::parse_2cp0(instruction, parser)
            }
            // 2 CP0 regs + 1 imm
            Instruction::AddImmediate
            | Instruction::AddImmediateUnsigned
            | Instruction::AndImmediate
            | Instruction::OrImmediate
            | Instruction::ShiftLeftLogical
            | Instruction::ShiftRightArithmetic
            | Instruction::ShiftRightLogical
            | Instruction::SetOnGtImmediate
            | Instruction::SetOnGtImmediateUnsigned
            | Instruction::SetOnLtImmediate
            | Instruction::SetOnLtImmediateUnsigned
            | Instruction::XorImmediate => Instruction::parse_2cp0_1imm(instruction, parser),
            // 2 CP0 regs + 1 lab
            Instruction::BranchOnEq
            | Instruction::BranchOnGt
            | Instruction::BranchOnGte
            | Instruction::BranchOnLt
            | Instruction::BranchOnLte
            | Instruction::BranchOnNotEq => Instruction::parse_2cp0_1lab(instruction, parser),
            // 1 CP0 reg
            Instruction::JumpRegister | Instruction::MoveFromHi | Instruction::MoveFromLo => {
                Instruction::parse_1cp0(instruction, parser)
            }
            // 1 CP0 reg + 1 CP1 reg
            Instruction::MoveFromC1 | Instruction::MoveToC1 => {
                Instruction::parse_1cp0_1cp1(instruction, parser)
            }
            // 1 CP0 reg + 1 imm
            Instruction::LoadImmediate | Instruction::LoadUpperImmediate => {
                Instruction::parse_1cp0_1imm(instruction, parser)
            }
            // 1 CP0 reg + 1 lab
            Instruction::BranchOnGteZero
            | Instruction::BranchOnGteZeroAndLink
            | Instruction::BranchOnGtZero
            | Instruction::BranchOnLteZero
            | Instruction::BranchOnLtZero
            | Instruction::BranchOnLtZeroAndLink
            | Instruction::LoadAddress => Instruction::parse_1cp0_1lab(instruction, parser),
            // 1 CP0 reg + 1 offset
            Instruction::LoadByte
            | Instruction::LoadByteUnsigned
            | Instruction::LoadHalfword
            | Instruction::LoadHalfwordUnsigned
            | Instruction::LoadWord
            | Instruction::StoreByte
            | Instruction::StoreConditional
            | Instruction::StoreHalfword
            | Instruction::StoreWord => Instruction::parse_1cp0_1off(instruction, parser),
            // 3 CP1 regs
            Instruction::FpAdditionDouble
            | Instruction::FpAdditionSingle
            | Instruction::FpDivideDouble
            | Instruction::FpDivideSingle
            | Instruction::FpMultiplyDouble
            | Instruction::FpMultiplySingle
            | Instruction::FpSubtractDouble
            | Instruction::FpSubtractSingle => Instruction::parse_3cp1(instruction, parser),
            // 2 CP1 regs
            Instruction::FpAbsoluteValueDouble
            | Instruction::FpAbsoluteValueSingle
            | Instruction::FpCeilingDoubleToInteger
            | Instruction::FpCeilingSingleToInteger
            | Instruction::FpCompareEqDouble
            | Instruction::FpCompareEqSingle
            | Instruction::FpCompareGtDouble
            | Instruction::FpCompareGtSingle
            | Instruction::FpCompareGteDouble
            | Instruction::FpCompareGteSingle
            | Instruction::FpCompareLtDouble
            | Instruction::FpCompareLtSingle
            | Instruction::FpCompareLteDouble
            | Instruction::FpCompareLteSingle
            | Instruction::FpConditionalMoveDoubleCp1
            | Instruction::FpConditionalMoveSingleCp1
            | Instruction::FpConvertSingleToDouble
            | Instruction::FpConvertIntegerToDouble
            | Instruction::FpConvertDoubleToSingle
            | Instruction::FpConvertIntegerToSingle
            | Instruction::FpConvertDoubleToInteger
            | Instruction::FpConvertSingleToInteger
            | Instruction::FpMoveDouble
            | Instruction::FpMoveSingle
            | Instruction::FpNegateDouble
            | Instruction::FpNegateSingle
            | Instruction::FpSquareRootDouble
            | Instruction::FpSquareRootSingle => Instruction::parse_2cp1(instruction, parser),
            // 1 CP1 reg + 1 off
            Instruction::FpLoadDouble
            | Instruction::FpLoadSingle
            | Instruction::FpStoreDouble
            | Instruction::FpStoreSingle => Instruction::parse_1cp1_1off(instruction, parser),
            // 1 lab
            Instruction::Jump
            | Instruction::JumpAndLink
            | Instruction::FpBranchOnConditionFalse
            | Instruction::FpBranchOnConditionTrue => Instruction::parse_1lab(instruction, parser),
            // No args
            Instruction::NoOp | Instruction::Syscall => Ok(Expression::NoArgs(instruction)),
        }
    }

    fn parse_3cp0(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let rd = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let rs = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let rt = get_cp0_reg(parser)?;
        Ok(Expression::Cp03(instruction, rd, rs, rt))
    }

    fn parse_2cp0(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let rd = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let rs = get_cp0_reg(parser)?;
        Ok(Expression::Cp02(instruction, rd, rs))
    }

    fn parse_2cp0_1imm(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let rd = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let rs = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let imm = get_int_imm(parser)?;
        Ok(Expression::Cp02Imm1(instruction, rd, rs, imm))
    }

    fn parse_2cp0_1lab(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let rd = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let rs = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let lab = get_lab(parser)?;
        Ok(Expression::Cp02Lab1(instruction, rd, rs, lab))
    }

    fn parse_1cp0(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let rs = get_cp0_reg(parser)?;
        Ok(Expression::Cp01(instruction, rs))
    }

    fn parse_1cp0_1cp1(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let rcp0 = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let rcp1 = get_cp1_reg(parser)?;
        Ok(Expression::Cp01Cp11(instruction, rcp0, rcp1))
    }

    fn parse_1cp0_1imm(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let rd = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let imm = get_int_imm(parser)?;
        Ok(Expression::Cp01Imm1(instruction, rd, imm))
    }

    fn parse_1cp0_1lab(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let r = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let lab = get_lab(parser)?;
        Ok(Expression::Cp01Lab1(instruction, r, lab))
    }

    fn parse_1cp0_1off(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let r = get_cp0_reg(parser)?;
        get_comma(parser)?;
        let (offset, offset_reg) = get_offset(parser)?;
        Ok(Expression::Cp01Off1(instruction, r, offset, offset_reg))
    }

    fn parse_3cp1(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let rd = get_cp1_reg(parser)?;
        get_comma(parser)?;
        let rs = get_cp1_reg(parser)?;
        get_comma(parser)?;
        let rt = get_cp1_reg(parser)?;
        Ok(Expression::Cp13(instruction, rd, rs, rt))
    }

    fn parse_2cp1(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let rd = get_cp1_reg(parser)?;
        get_comma(parser)?;
        let rs = get_cp1_reg(parser)?;
        Ok(Expression::Cp12(instruction, rd, rs))
    }

    fn parse_1cp1_1off(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let r = get_cp1_reg(parser)?;
        get_comma(parser)?;
        let (offset, offset_reg) = get_offset(parser)?;
        Ok(Expression::Cp01Off1(instruction, r, offset, offset_reg))
    }

    fn parse_1lab(instruction: Instruction, parser: &mut Parser) -> Result<Expression, Error> {
        let lab = get_lab(parser)?;
        Ok(Expression::Lab1(instruction, lab))
    }
}

fn get_comma(parser: &mut Parser) -> Result<(), Error> {
    match parser
        .next()
        .ok_or(Error::MissingComma(lspan_to_csspan(parser.lexer.span())))?
    {
        Token::Comma => Ok(()),
        other => Err(Error::UnexpectedToken {
            got: other,
            span: parser.lexer.span(),
            expected: "comma",
        }),
    }
}

fn get_cp0_reg(parser: &mut Parser) -> Result<Register, Error> {
    let token = parser.next().ok_or(Error::Eof)?;
    match &token {
        Token::Register(reg) => {
            if reg.is_fp_reg() {
                Err(Error::IncorrectCoprocRegister {
                    got: "1",
                    expected: "0",
                    span: parser.lexer.span(),
                })
            } else {
                Ok(*reg)
            }
        }
        _ => Err(Error::UnexpectedToken {
            got: token,
            span: parser.lexer.span(),
            expected: "CP0 register",
        }),
    }
}

fn get_int_imm(parser: &mut Parser) -> Result<i32, Error> {
    let token = parser.next().ok_or(Error::Eof)?;
    match &token {
        Token::NumberImmediate(string) => match parse_immediate(&string) {
            Some(int) => Ok(int),
            None => Err(Error::InvalidIntegerLiteral(token, parser.lexer.span())),
        },
        _ => Err(Error::UnexpectedToken {
            got: token,
            span: parser.lexer.span(),
            expected: "integer literal",
        }),
    }
}

fn get_lab(parser: &mut Parser) -> Result<String, Error> {
    match parser.next().ok_or(Error::Eof)? {
        Token::Identifier(Identifier::Label(label)) => Ok(label),
        other => Err(Error::UnexpectedToken {
            got: other,
            span: parser.lexer.span(),
            expected: "label",
        }),
    }
}

fn get_cp1_reg(parser: &mut Parser) -> Result<Register, Error> {
    let token = parser.next().ok_or(Error::Eof)?;
    match &token {
        Token::Register(reg) => {
            if !reg.is_fp_reg() {
                Err(Error::IncorrectCoprocRegister {
                    got: "0",
                    expected: "1",
                    span: parser.lexer.span(),
                })
            } else {
                Ok(*reg)
            }
        }
        _ => Err(Error::UnexpectedToken {
            got: token,
            span: parser.lexer.span(),
            expected: "CP1 register",
        }),
    }
}

fn get_offset(parser: &mut Parser) -> Result<(i32, Register), Error> {
    let token = parser.next().ok_or(Error::Eof)?;
    match &token {
        Token::OffsetFrom(offset) => Ok(*offset),
        _ => Err(Error::UnexpectedToken {
            got: token,
            span: parser.lexer.span(),
            expected: "offset",
        }),
    }
}
