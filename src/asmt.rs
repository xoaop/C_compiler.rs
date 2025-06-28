use std::collections::HashMap;

use crate::{symbol, tacky};

#[derive(Debug)]
pub struct Program {
    pub top_level_defs: Vec<TopLevel>,
}

#[derive(Debug)]
pub struct Function {
    pub identifier: String,
    pub instructions: Vec<Instruction>,
    pub global: bool,
}

#[derive(Debug)]
pub struct StaticVariable {
    pub name: String,
    pub global: bool,
    pub init: i32,
}

#[derive(Debug)]
pub enum TopLevel {
    Function(Function),
    StaticVariable(StaticVariable),
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
    DeallocateStack(Operand),

    Push(Operand),
    Call(String),

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
    Data(String),
}

#[derive(Debug, Clone)]
pub enum Register {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
}

const ARG_REGISTERS: [Operand; 6] = [
    Operand::Reg(Register::DI),
    Operand::Reg(Register::SI),
    Operand::Reg(Register::DX),
    Operand::Reg(Register::CX),
    Operand::Reg(Register::R8),
    Operand::Reg(Register::R9),
];

#[derive(Debug, Clone, Copy)]
pub enum RegisterWidth {
    W8,  // 8位
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
pub struct Asmt<'global> {
    name_map: HashMap<String, i32>,

    current_function_scope: String,
    stack_size_map: std::collections::HashMap<String, i32>,

    symbol_table: &'global mut HashMap<String, symbol::SymbolInfo>,
}

impl<'global> Asmt<'global> {
    pub fn new(symbol_table: &'global mut HashMap<String, symbol::SymbolInfo>) -> Self {
        Asmt {
            name_map: HashMap::new(),
            current_function_scope: "there is no function name@#$%^&*+".to_string(),
            stack_size_map: std::collections::HashMap::<String, i32>::new(),
            symbol_table: symbol_table,
        }
    }

    fn pseudo_to_memory(&mut self, identifier: &str) -> Operand {
        if self.name_map.contains_key(identifier) {
            return Operand::Stack(self.name_map.get(identifier).cloned().expect("Error!"));
        }

        if let Some(symbol_info) = self.symbol_table.get(identifier) {
            if let symbol::IdentifierAttrs::StaticAttr { .. } = &symbol_info.attrs {
                return Operand::Data(identifier.to_string());
            }
        }

        *self
            .stack_size_map
            .get_mut(&self.current_function_scope)
            .unwrap() += std::mem::size_of::<i32>() as i32;
        self.name_map.insert(
            identifier.to_string(),
            -self
                .stack_size_map
                .get(&self.current_function_scope)
                .unwrap(),
        );
        return Operand::Stack(
            -self
                .stack_size_map
                .get(&self.current_function_scope)
                .unwrap(),
        );
    }

    fn fix_operand(&mut self, op: &Operand) -> Operand {
        match op {
            Operand::Pseudo(ident) => self.pseudo_to_memory(&ident),
            _ => op.clone(),
        }
    }

    fn fix_allocate_stack(alloc_stack: Instruction) -> Option<Instruction> {
        if let Instruction::AllocateStack(op) = alloc_stack {
            match op {
                Operand::Imm(size) => {
                    if size == 0 {
                        return None;
                    }

                    let mut size = size;
                    size += 16 - size % 16;

                    return Some(Instruction::AllocateStack(Operand::Imm(size)));
                }
                _ => {
                    panic!("");
                }
            }
        } else {
            panic!("");
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
                            if matches!(src, Operand::Stack(_)) || matches!(src, Operand::Data(_)) {
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
                            if matches!(dst, Operand::Stack(_)) || matches!(dst, Operand::Data(_)) {
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

                    if matches!(new_src, Operand::Stack(_)) || matches!(new_src, Operand::Data(_)) {
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
                    if matches!(dst, Operand::Stack(_)) || matches!(dst, Operand::Data(_)) {
                        insts.push(Instruction::Mov(
                            new_dst.clone(),
                            Operand::Reg(Register::R11),
                        ));
                        new_dst = Operand::Reg(Register::R11);
                    }

                    insts.push(Instruction::SetCC(cond_code.clone(), new_dst));
                    insts.push(Instruction::Mov(Operand::Reg(Register::R11), dst.clone()));
                }

                // Instruction::AllocateStack(op) => match op {
                //     Operand::Imm(stack_alloc_size) => {
                //         let mut stack_alloc_size = stack_alloc_size.clone();
                //         if stack_alloc_size != 0 {
                //             stack_alloc_size += 16 - stack_alloc_size % 16;
                //             insts.push(Instruction::AllocateStack(Operand::Imm(stack_alloc_size)));
                //         }

                //     }
                //     _ => {
                //         panic!("");
                //     }
                // }
                _ => {
                    insts.push(inst);
                }
            }
        }

        insts
    }

    fn emit_instructions(
        &mut self,
        body: &Vec<tacky::Instruction>,
        mut instructions: Vec<Instruction>,
    ) -> Vec<Instruction> {
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

                tacky::Instruction::FunCall(name, params, dst) => {
                    let mut register_args = params.clone();

                    let mut stack_args = Vec::<tacky::Operand>::new();

                    if register_args.len() > 6 {
                        stack_args = register_args.split_off(6 - 1);
                    }

                    let mut stack_padding = 0;
                    //如果栈参数个数为奇数，就要padding
                    if stack_args.len() % 2 == 1 {
                        stack_padding = 8;
                    } else {
                        stack_padding = 0;
                    }

                    if stack_padding != 0 {
                        instructions.push(Instruction::AllocateStack(Operand::Imm(stack_padding)));
                    }

                    let mut reg_idx = 0;
                    for arg in register_args {
                        instructions
                            .push(Instruction::Mov(arg.into(), ARG_REGISTERS[reg_idx].clone()));
                        reg_idx += 1;
                    }

                    let stack_args_len = stack_args.len();

                    stack_args.reverse();
                    for arg in stack_args {
                        let arg: Operand = arg.into();

                        match arg {
                            Operand::Imm(..) | Operand::Reg(..) => {
                                instructions.push(Instruction::Push(arg.clone()));
                            }

                            _ => {
                                instructions
                                    .push(Instruction::Mov(arg, Operand::Reg(Register::AX)));
                                instructions.push(Instruction::Push(Operand::Reg(Register::AX)));
                            }
                        }
                    }

                    instructions.push(Instruction::Call(name.clone()));

                    //函数调用完就把用来传参和padding的栈给清空了
                    let bytes_to_removed = 8 * (stack_args_len as i32) + stack_padding;
                    if bytes_to_removed != 0 {
                        instructions
                            .push(Instruction::DeallocateStack(Operand::Imm(bytes_to_removed)));
                    }

                    instructions.push(Instruction::Mov(
                        Operand::Reg(Register::AX),
                        dst.clone().into(),
                    ));
                }

                _ => {
                    eprintln!("Unsupported tacky::Instruction: {:?}", instruction);
                    std::process::exit(1);
                }
            }
        }
        return instructions;
    }

    fn emit_function(&mut self, function: &tacky::Function) -> Function {
        let identifier = function.name.clone();

        self.current_function_scope = identifier.clone();

        self.stack_size_map
            .insert(self.current_function_scope.clone(), 0);

        let mut instructions = Vec::<Instruction>::new();

        for (i, param) in function.params.iter().enumerate() {
            if i < ARG_REGISTERS.len() {
                // 把函数在寄存器上的参数都复制到栈上
                instructions.push(Instruction::Mov(
                    ARG_REGISTERS[i].clone(),
                    Operand::Pseudo(param.clone()),
                ));
            } else {
                // 再把函数在栈上的参数(在调用者栈帧里)都复制到自己的栈帧里
                // 参数在调用者栈帧, 第7个参数位置为Stack(16), 第8个为Stack(24), ...
                let stack_offset = 16 + ((i - ARG_REGISTERS.len()) as i32) * 8;
                instructions.push(Instruction::Mov(
                    Operand::Stack(stack_offset),
                    Operand::Pseudo(param.clone()),
                ));
            }
        }

        instructions = self.emit_instructions(&function.body, instructions);

        instructions = self.fix_illegal_instruction(instructions);

        let stack_alloc_size = self
            .stack_size_map
            .get(&self.current_function_scope)
            .unwrap()
            .clone();

        let fixed_alloc_stack = Self::fix_allocate_stack(Instruction::AllocateStack(Operand::Imm(stack_alloc_size)));

        if fixed_alloc_stack.is_some() {
            instructions.insert(
                0,
                fixed_alloc_stack.unwrap(),
            );
        }

        Function {
            identifier: identifier,
            instructions: instructions,
            global: function.global,
        }
    }

    fn emit_static_var(&mut self, static_var: &tacky::StaticVariable) -> StaticVariable {
        return StaticVariable {
            name: static_var.name.clone(),
            global: static_var.global,
            init: static_var.init,
        };
    }

    pub fn emit_program(&mut self, program: &tacky::Program) -> Program {
        let mut top_level_defs = Vec::<TopLevel>::new();
        for top_level in &program.top_levels {
            match top_level {
                tacky::TopLevel::Function(func) => {
                    top_level_defs.push(TopLevel::Function(self.emit_function(func)));
                }

                tacky::TopLevel::StaticVariable(static_var) => {
                    top_level_defs.push(TopLevel::StaticVariable(self.emit_static_var(static_var)));
                }
            }
        }

        return Program {
            top_level_defs: top_level_defs,
        };
    }
}
