use core::fmt;
use crate::{ast, tacky};

#[derive(Debug)]
pub struct Asmt {
    pub program: Program,
}

#[derive(Debug)]
pub struct Program {
    pub function_definition: Function,
}

#[derive(Debug)]
pub struct Function {
    pub identifier: String, 
    pub instructions: Vec<Instruction>,
}



#[derive(Debug)]
pub enum Instruction {
    Mov(Operand, Operand), 
    Unray(Operator, Operand),
    AllocateStack(Operand), // 分配栈空间，相当于给rsp（栈顶指针）减去一个数
    Ret,
}

#[derive(Debug)]
enum Operator {
    Neg, // -
    Not, // ~
}

#[derive(Debug)]
pub enum Operand {
    Imm(i32), 
    Reg(Register),
    Pseudo(String), //虚拟寄存器
    Stack(i32), // 获取 $(i64)(rbp)
}

#[derive(Debug)]
pub enum Register {
    AX,
    R10,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mov(src, dst) => write!(f, "movl\t{}, {}", src, dst),
            Instruction::Ret => write!(f, "movq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret"),
            Instruction::Unray(unray_op, operand) => write!(f, "{}\t{}", unray_op, operand),
            Instruction::AllocateStack(size) => write!(f, "subq\t{}, %rsp", size),

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
            Register::R10 => write!(f, "%r10d"),
            Register::AX => write!(f, "%eax"),
            _ => {
                std::process::exit(1);
            }
        }
    }
}


static mut stack_alloc_size: i32 = 0;

fn pseudo_to_stack(identifier: &str) -> i32 {
    if let Some(num_str) = identifier.strip_prefix("tmp.") {
        let offset = -1 * (std::mem::size_of::<i32>() as i32) * (num_str.parse::<i32>().ok().unwrap() + 1);

        unsafe {
            if offset < stack_alloc_size {
                stack_alloc_size = offset;
            }
            return offset;
        }

    } else {
        std::process::exit(1);
    }
}

fn emit_operand(operand: &tacky::Operand) -> Operand {
    let op1 = match operand {
        tacky::Operand::Constant(val) => Operand::Imm(val.clone()),
        tacky::Operand::Var(id) => {
            Operand::Stack(pseudo_to_stack(id))
        },
    };

    op1
}

fn emit_operator(operator: &tacky::Operator) -> Operator {
    match operator {
        tacky::Operator::Complement => Operator::Not,
        tacky::Operator::Negate => Operator::Neg,
    }
}


fn fix_illegal_instruction(instruction: Instruction) -> Vec<Instruction> {
    let mut insts = Vec::<Instruction>::new();
    
    match instruction {
        Instruction::Mov(src, dst) => {
            match src {
                Operand::Stack(_) => {
                    insts.push(Instruction::Mov(src, Operand::Reg(Register::R10)));
                    insts.push(Instruction::Mov(Operand::Reg(Register::R10), dst));
                }
                _ => {
                    insts.push(Instruction::Mov(src, dst));
                }
            }
        },
        _ => {
            insts.push(instruction);
        },
    }

    insts
}

fn emit_instructions(body: &Vec<tacky::Instruction>) ->Vec<Instruction> {
    let mut instructions = Vec::<Instruction>::new();

    // State 1
    for instruction in body.iter() {
        match instruction {
            tacky::Instruction::Return(operand) => {
                let inst = Instruction::Mov(emit_operand(operand), Operand::Reg(Register::AX));

                instructions.extend(fix_illegal_instruction(inst));
            
                instructions.push(Instruction::Ret);
            }
            tacky::Instruction::Unary(unray_operator, src, dst) => {
                instructions.extend(fix_illegal_instruction(Instruction::Mov(emit_operand(src), emit_operand(dst))));
                instructions.push(Instruction::Unray(emit_operator(unray_operator), emit_operand(dst)));
            }
            // No other instruction variants to handle.
        }
    }
    //State 2



    instructions
}


fn emit_function(function: &tacky::Function) -> Function {
    let identifier = function.name.clone();

    let mut instructions = emit_instructions(&function.body);


    unsafe {
        instructions.insert(0, Instruction::AllocateStack(Operand::Imm(-stack_alloc_size)));
    }


    Function { identifier: identifier, instructions: instructions }
}

pub fn emit_program(program: &tacky::Program) -> Program {
    Program { function_definition: emit_function(&program.function_def) }
}


