use std::{collections::HashMap, fmt::Binary};

use crate::tacky;

#[derive(Debug)]
pub struct Program {
    pub function_definition: Function,
}

#[derive(Debug)]
pub struct Function {
    pub identifier: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov(Operand, Operand),
    Unray(Operator, Operand),
    Binary(Operator, Operand, Operand),

    Cmp(Operand, Operand),
    //devide is special
    Idiv(Operand),
    Cdq,

    Jmp(String),
    JmpCC(CondCode, String),
    SetCC(CondCode, Operand),
    Label(String),

    AllocateStack(Operand), // 分配栈空间，相当于给rsp（栈顶指针）减去一个数
    Ret,
}

#[derive(Debug, Clone)]
pub enum Operator {
    Neg, // -
    Not, // ~

    Add,
    Sub,
    Mul,
}

#[derive(Debug, Clone)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

impl CondCode {
    pub fn emit_condcode(relational_operator: tacky::Operator) -> Self {
        match relational_operator {
            tacky::Operator::Equal => CondCode::E,
            tacky::Operator::NotEqual => CondCode::NE,
            tacky::Operator::GreaterThan => CondCode::G,
            tacky::Operator::GreaterOrEqual => CondCode::GE,
            tacky::Operator::LessThan => CondCode::L,
            tacky::Operator::LessOrEqual => CondCode::LE,
            _ => {
                panic!("未知的condcode!");
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Register),
    Pseudo(String), //虚拟寄存器
    Stack(i32),     // 获取 $(i64)(rbp)
}

#[derive(Debug, Clone)]
pub enum Register {
    AX,
    DX,
    R10,
    R11,
}

#[derive(Debug, Clone, Copy)]
pub enum RegisterWidth {
    W8,  // 8位
    W16, // 16位
    W32, // 32位
    W64, // 64位
}

impl From<tacky::Operand> for Operand {
    fn from(value: tacky::Operand) -> Self {
        match value {
            tacky::Operand::Constant(val) => Operand::Imm(val),
            tacky::Operand::Var(id) => Operand::Pseudo(id.clone()),
            _ => {
                panic!("It shouldn't appear tacky::Operand::Nothing!");
            }
        }
    }
}

impl From<tacky::Operator> for Operator {
    fn from(operator: tacky::Operator) -> Self {
        match operator {
            tacky::Operator::Complement => Operator::Not,
            tacky::Operator::Negate => Operator::Neg,
            tacky::Operator::Add => Operator::Add,
            tacky::Operator::Sub => Operator::Sub,
            tacky::Operator::Mul => Operator::Mul,
            _ => {
                todo!()
            }
        }
    }
}

#[derive(Debug)]
pub struct Asmt {
    name_map: HashMap<String, i32>,
    stack_alloc_size: i32,
}

impl Asmt {
    pub fn new() -> Self {
        Asmt {
            name_map: HashMap::new(),
            stack_alloc_size: 0,
        }
    }

    fn pseudo_to_stack(&mut self, identifier: &str) -> Operand {
        if self.name_map.contains_key(identifier) {
            return Operand::Stack(self.name_map.get(identifier).cloned().expect("Error!"));
        }

        self.stack_alloc_size += std::mem::size_of::<i32>() as i32;
        self.name_map
            .insert(identifier.to_string(), -self.stack_alloc_size);
        return Operand::Stack(-self.stack_alloc_size);
    }

    fn fix_operand(&mut self, op: &Operand) -> Operand {
        match op {
            Operand::Pseudo(ident) => self.pseudo_to_stack(&ident),
            _ => op.clone(),
        }
    }

    //目前是用来修复如mov (%rax), (%rbx) 此类从内存到内存的操作
    fn fix_illegal_instruction(&mut self, instructions: Vec<Instruction>) -> Vec<Instruction> {
        let mut insts = Vec::<Instruction>::new();

        for inst in instructions {
            match &inst {
                Instruction::Mov(src, dst) => {
                    let src = self.fix_operand(src);
                    let dst = self.fix_operand(dst);

                    insts.push(Instruction::Mov(src.clone(), Operand::Reg(Register::R10)));
                    insts.push(Instruction::Mov(Operand::Reg(Register::R10), dst.clone()));
                }
                Instruction::Idiv(src2) => {
                    let src2 = self.fix_operand(src2);

                    insts.push(Instruction::Mov(src2.clone(), Operand::Reg(Register::R10)));
                    insts.push(Instruction::Idiv(Operand::Reg(Register::R10)));
                }
                Instruction::Binary(op, src, dst) => {
                    let src = self.fix_operand(src);
                    let dst = self.fix_operand(dst);

                    let inst = Instruction::Binary(op.clone(), src.clone(), dst.clone());

                    match op {
                        Operator::Add | Operator::Sub => {
                            if matches!(src, Operand::Stack(_)) {
                                insts.push(Instruction::Mov(
                                    src.clone(),
                                    Operand::Reg(Register::R10),
                                ));
                                insts.push(Instruction::Binary(
                                    op.clone(),
                                    Operand::Reg(Register::R10),
                                    dst.clone(),
                                ));
                            } else {
                                insts.push(inst);
                            }
                        }
                        Operator::Mul => {
                            if matches!(dst, Operand::Stack(_)) {
                                insts.push(Instruction::Mov(
                                    dst.clone(),
                                    Operand::Reg(Register::R11),
                                ));
                                insts.push(Instruction::Binary(
                                    op.clone(),
                                    src.clone(),
                                    Operand::Reg(Register::R11),
                                ));
                                insts.push(Instruction::Mov(
                                    Operand::Reg(Register::R11),
                                    dst.clone(),
                                ));
                            } else {
                                insts.push(inst);
                            }
                        }
                        _ => {
                            insts.push(inst);
                        }
                    }
                }
                Instruction::Cmp(src, dst) => {
                    let src = self.fix_operand(src);
                    let dst = self.fix_operand(dst);

                    let mut new_src = src.clone();
                    let mut new_dst = dst.clone();

                    if matches!(new_src, Operand::Stack(_)) {
                        insts.push(Instruction::Mov(
                            new_src.clone(),
                            Operand::Reg(Register::R10),
                        ));
                        new_src = Operand::Reg(Register::R10);
                    }

                    if matches!(new_dst, Operand::Imm(_)) {
                        insts.push(Instruction::Mov(dst.clone(), Operand::Reg(Register::R11)));
                        new_dst = Operand::Reg(Register::R11);
                    }

                    insts.push(Instruction::Cmp(new_src, new_dst));
                }
                Instruction::SetCC(cond_code, dst) => {
                    let dst = self.fix_operand(dst);

                    let mut new_dst = dst.clone();
                    if matches!(dst, Operand::Stack(_)) {
                        insts.push(Instruction::Mov(
                            new_dst.clone(),
                            Operand::Reg(Register::R11),
                        ));
                        new_dst = Operand::Reg(Register::R11);
                    }

                    insts.push(Instruction::SetCC(cond_code.clone(), new_dst));
                    insts.push(Instruction::Mov(Operand::Reg(Register::R11), dst.clone()));
                }

                _ => {
                    insts.push(inst);
                }
            }
        }

        insts
    }

    fn emit_instructions(&mut self, body: &Vec<tacky::Instruction>) -> Vec<Instruction> {
        let mut instructions = Vec::<Instruction>::new();

        for instruction in body.iter() {
            match instruction {
                tacky::Instruction::Return(operand) => {
                    instructions.push(Instruction::Mov(
                        operand.clone().into(),
                        Operand::Reg(Register::AX),
                    ));

                    instructions.push(Instruction::Ret);
                }
                tacky::Instruction::Unary(unray_operator, src, dst) => match unray_operator {
                    tacky::Operator::Not => {
                        instructions.push(Instruction::Cmp(Operand::Imm(0), src.clone().into()));
                        instructions.push(Instruction::Mov(Operand::Imm(0), dst.clone().into()));
                        instructions.push(Instruction::SetCC(CondCode::E, dst.clone().into()));
                    }

                    _ => {
                        instructions.push(Instruction::Mov(src.clone().into(), dst.clone().into()));
                        instructions.push(Instruction::Unray(
                            unray_operator.clone().into(),
                            dst.clone().into(),
                        ));
                    }
                },
                tacky::Instruction::Binary(bin_op, src1, src2, dst) => match bin_op {
                    tacky::Operator::Div => {
                        instructions.push(Instruction::Mov(
                            src1.clone().into(),
                            Operand::Reg(Register::AX),
                        ));
                        instructions.push(Instruction::Cdq);
                        instructions.push(Instruction::Idiv(src2.clone().into()));
                        instructions.push(Instruction::Mov(
                            Operand::Reg(Register::AX),
                            dst.clone().into(),
                        ));
                    }
                    tacky::Operator::Remainder => {
                        instructions.push(Instruction::Mov(
                            src1.clone().into(),
                            Operand::Reg(Register::AX),
                        ));
                        instructions.push(Instruction::Cdq);
                        instructions.push(Instruction::Idiv(src2.clone().into()));
                        instructions.push(Instruction::Mov(
                            Operand::Reg(Register::DX),
                            dst.clone().into(),
                        ));
                    }
                    tacky::Operator::GreaterThan
                    | tacky::Operator::GreaterOrEqual
                    | tacky::Operator::LessThan
                    | tacky::Operator::LessOrEqual
                    | tacky::Operator::Equal
                    | tacky::Operator::NotEqual => {
                        let cond_code = CondCode::emit_condcode(bin_op.clone());
                        instructions
                            .push(Instruction::Cmp(src2.clone().into(), src1.clone().into()));
                        instructions.push(Instruction::Mov(Operand::Imm(0), dst.clone().into()));
                        instructions.push(Instruction::SetCC(cond_code, dst.clone().into()));
                    }
                    _ => {
                        instructions
                            .push(Instruction::Mov(src1.clone().into(), dst.clone().into()));
                        instructions.push(Instruction::Binary(
                            bin_op.clone().into(),
                            src2.clone().into(),
                            dst.clone().into(),
                        ));
                    }
                },
                tacky::Instruction::Jump(target) => {
                    instructions.push(Instruction::Jmp(target.to_string()));
                }

                tacky::Instruction::JumpIfZero(val, target) => {
                    instructions.push(Instruction::Cmp(Operand::Imm(0), val.clone().into()));
                    instructions.push(Instruction::JmpCC(CondCode::E, target.to_string()));
                }

                tacky::Instruction::JumpIfNotZero(val, target) => {
                    instructions.push(Instruction::Cmp(Operand::Imm(0), val.clone().into()));
                    instructions.push(Instruction::JmpCC(CondCode::NE, target.to_string()));
                }

                tacky::Instruction::Copy(src, dst) => {
                    instructions.push(Instruction::Mov(src.clone().into(), dst.clone().into()));
                }

                tacky::Instruction::Label(id) => {
                    instructions.push(Instruction::Label(id.to_string()));
                }

                _ => {
                    eprintln!("Unsupported tacky::Instruction: {:?}", instruction);
                    std::process::exit(1);
                }
            }
        }

        self.fix_illegal_instruction(instructions)
    }

    fn emit_function(&mut self, function: &tacky::Function) -> Function {
        let identifier = function.name.clone();

        let mut instructions = self.emit_instructions(&function.body);

        instructions.insert(
            0,
            Instruction::AllocateStack(Operand::Imm(self.stack_alloc_size)),
        );

        Function {
            identifier: identifier,
            instructions: instructions,
        }
    }

    pub fn emit_program(&mut self, program: &tacky::Program) -> Program {
        Program {
            function_definition: self.emit_function(&program.function_def),
        }
    }
}
