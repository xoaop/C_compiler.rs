use crate::asmt;
use std::{fs::File};
use std::io::Write;

// Bring required types into scope
use crate::asmt::{CondCode, Instruction, Operand, Operator, Register, RegisterWidth};

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

            Instruction::SetCC(cond_code, operand) => write!(f, "set{}\t{}", cond_code, operand.fmt_with_width(RegisterWidth::W8)),
            
            Instruction::Label(label) => write!(f, ".L{}:", label),

            Instruction::DeallocateStack(op) => {
                write!(f, "addq\t{}, %rsp", op)
            }

            Instruction::Push(op) => {
                write!(f, "pushq\t{}", op.fmt_with_width(RegisterWidth::W64))
            }

            Instruction::Call(name) => {
                write!(f, "call\t{}", name)
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
                panic!("");
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
                panic!("");
            }
        }
    }
}

impl Operand {
    fn fmt_with_width(&self, width: RegisterWidth) -> String {
        match self {
            Operand::Imm(i32) => format!("${}", i32),
            Operand::Reg(reg) => format!("{}", reg.fmt_with_width(Some(width))),
            Operand::Stack(offset) => format!("{}(%rbp)", offset),

            _ => {
                panic!("");
            }
        }
    }
}

impl fmt::Display for Register {

    //生成所有寄存器各自名字的基底base
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::AX => write!(f, "%eax"),
            Register::CX => write!(f, "%ecx"),
            Register::DX => write!(f, "%edx"),
            Register::SI => write!(f, "%esi"),
            Register::DI => write!(f, "%edi"),
            Register::R8 => write!(f, "%r8d"),
            Register::R9 => write!(f, "%r9d"),
            Register::R10 => write!(f, "%r10d"),
            Register::R11 => write!(f, "%r11d"),
        }
    }
}

impl Register {

    fn get_base(&self) -> String {
        match self {
            Register::AX => format!("%a"),
            Register::CX => format!("%c"),
            Register::DX => format!("%d"),
            Register::SI => format!("%si"),
            Register::DI => format!("%di"),
            Register::R8 => format!("%r8"),
            Register::R9 => format!("%r9"),
            Register::R10 => format!("%r10"),
            Register::R11 => format!("%r11"),
        }
    }

    //基于基底base和字长来生成完整的寄存器名字
    pub fn fmt_with_width(&self, mut width: Option<RegisterWidth>) -> String {
 
        if width.is_none() {
            width = Some(RegisterWidth::W32);
        }
        let width = width.unwrap();

        let base = self.get_base();
        let reg_str = match self {
            Register::AX | Register::CX | Register::DX | Register::DI | Register::SI => {
                match width {
                    RegisterWidth::W8 => format!("{}l", base),
                    RegisterWidth::W32 => format!("e{}x", base),
                    RegisterWidth::W64 => format!("r{}x", base),
                }
            }

            Register::R8 | Register::R9 | Register::R10 | Register::R11 => {
                match width {
                    RegisterWidth::W8 => format!("{}b", base),
                    RegisterWidth::W32 => format!("{}d", base),
                    RegisterWidth::W64 => format!("{}", base),
                }
            }
        };

        format!("{}", reg_str)
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

pub fn emit_asm(assembly_ast: &asmt::Program, path: &String) {
    let file_path = if let Some(idx) = path.rfind('.') {
        let mut new_path = path.clone();
        new_path.replace_range(idx.., ".S");
        new_path
    } else {
        format!("{}.S", path)
    };
    
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

    for function_def in program.function_definitions.iter() {
        result.push_str(&generate_at_function(&function_def));
    }

    
    //NOTE: 在windows不需要
    if cfg!(target_os = "linux") {
        result.push_str(".section .note.GNU-stack,\"\",@progbits\n");
    }

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
