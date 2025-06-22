use crate::asmt;
use std::fs::File;
use std::io::Write;

// Bring required types into scope
use crate::asmt::{Instruction, Operator, Operand, Register, RegisterWidth, CondCode};

use std::fmt;


impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mov(src, dst) => write!(f, "movl\t{}, {}", src, dst),
            Instruction::Ret => write!(f, "movq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"),
            Instruction::Unray(unray_op, operand) => write!(f, "{}\t{}", unray_op, operand),
            Instruction::AllocateStack(size) => write!(f, "subq\t{}, %rsp", size),
            Instruction::Binary(op, src, dst) => write!(f, "{}\t{}, {}", op, src, dst),
            Instruction::Idiv(operand) => write!(f, "idivl\t{}", operand),
            Instruction::Cdq => write!(f, "cdq"),
            Instruction::Cmp(src, dst) => write!(f, "cmpl\t{}, {}", src, dst),
            Instruction::Jmp(label) => write!(f, "jmp\t.L{}", label),
            Instruction::JmpCC(cond_code, label) => write!(f, "j{}\t.L{}", cond_code, label),
            Instruction::SetCC(cond_code, operand) => {
                match operand {
                    Operand::Reg(reg) => {
                        write!(f, "set{}\t", cond_code)?;
                        reg.fmt_with_width(RegisterWidth::W8, f)
                    },
                    _ => write!(f, "set{}\t{}", cond_code, operand),
                }
            },
            Instruction::Label(label) => write!(f, ".L{}:", label),
            _ => {
                std::process::exit(1);
            }
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Neg => write!(f, "negl"),
            Operator::Not => write!(f, "notl"),
            Operator::Add => write!(f, "addl"),
            Operator::Sub => write!(f, "subl"),
            Operator::Mul => write!(f, "imull"),

            _ => {
                std::process::exit(1);
            }
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Imm(i32) => write!(f, "${}", i32),
            Operand::Reg(reg) => write!(f, "{}", reg),
            Operand::Stack(offset) => write!(f, "{}(%rbp)", offset),

            _ => {
                std::process::exit(1);
            }
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::AX => write!(f, "%eax"),
            Register::DX => write!(f, "%edx"),
            Register::R10 => write!(f, "%r10d"),
            Register::R11 => write!(f, "%r11d"),
            _ => {
                std::process::exit(1);
            }
        }
    }
}

impl Register {
    pub fn fmt_with_width(&self, width: RegisterWidth, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self, width) {
            (Register::AX, RegisterWidth::W8) => write!(f, "%al"),
            (Register::AX, RegisterWidth::W16) => write!(f, "%ax"),
            (Register::AX, RegisterWidth::W32) => write!(f, "%eax"),
            (Register::AX, RegisterWidth::W64) => write!(f, "%rax"),
            (Register::DX, RegisterWidth::W8) => write!(f, "%dl"),
            (Register::DX, RegisterWidth::W16) => write!(f, "%dx"),
            (Register::DX, RegisterWidth::W32) => write!(f, "%edx"),
            (Register::DX, RegisterWidth::W64) => write!(f, "%rdx"),
            (Register::R10, RegisterWidth::W32) => write!(f, "%r10b"),
            (Register::R10, RegisterWidth::W8) => write!(f, "%r10d"),
            (Register::R11, RegisterWidth::W32) => write!(f, "%r11d"),
            (Register::R11, RegisterWidth::W8) => write!(f, "%r11b"),
            _ => std::process::exit(1),
        }
    }
}

impl fmt::Display for CondCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CondCode::E => write!(f, "e"),
            CondCode::NE => write!(f, "ne"),
            CondCode::G => write!(f, "g"),
            CondCode::GE => write!(f, "ge"),
            CondCode::L => write!(f, "l"),
            CondCode::LE => write!(f, "le"),
            _ => {
                std::process::exit(1);
            }
        }
    }
}




pub fn generate_assembly(assembly_ast: &asmt::Program) {
    let file_path = "output.s";
    let mut file = match File::create(file_path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Failed to create file: {}", e);
            return;
        }
    };

    let result = generate_at_program(&assembly_ast);

    if let Err(e) = file.write(result.as_bytes()) {
        eprintln!("Failed to write to file: {}", e);
    }
}

fn generate_at_program(program: &asmt::Program) -> String {
    let mut result = String::new();

    result.push_str(&generate_at_function(&program.function_definition));

    result.push_str(".section .note.GNU-stack,\"\",@progbits\n");

    return result;
}

fn generate_at_function(function: &asmt::Function) -> String {
    let mut result = String::new();

    result.push_str(
        format!(
            "\t.globl {}\n{}:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n",
            function.identifier, function.identifier
        )
        .as_str(),
    );

    for inst in function.instructions.iter() {
        result.push_str(format!("\t{}\n", inst).as_str());
    }

    return result;
}
