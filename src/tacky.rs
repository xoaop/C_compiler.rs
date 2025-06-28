use std::collections::HashMap;

use crate::global::VariableContext;
use crate::parse::ast;
use crate::symbol;

#[derive(Debug, Clone)]
pub enum Instruction {
    Return(Operand),
    Unary(Operator, Operand, Operand),
    Binary(Operator, Operand, Operand, Operand),
    Copy(Operand, Operand),
    Jump(String),
    JumpIfZero(Operand, String),
    JumpIfNotZero(Operand, String),
    Label(String),
    FunCall(String, Vec<Operand>, Operand), // fun_name args dst
}

#[derive(Debug, Clone)]
pub enum Operator {
    //singal
    Negate,
    Complement,
    Not,

    //Binary
    Add,
    Sub,
    Mul,
    Div,
    Remainder,

    //relational_operator
    Equal,
    NotEqual,
    GreaterThan,
    GreaterOrEqual,
    LessThan,
    LessOrEqual,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Constant(i32),
    Var(String),
    Nothing,
}

#[derive(Debug)]
pub struct Program {
    pub top_levels: Vec<TopLevel>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Instruction>,
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
    StaticVariable(StaticVariable),
    Function(Function),
}

//TODO: 重构LabelPrefix
pub enum LabelPrefix {
    True,
    False,
    Else,
    End,
    Break,
    Continue,
}

impl std::fmt::Display for LabelPrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LabelPrefix::True => write!(f, "true"),
            LabelPrefix::False => write!(f, "false"),
            LabelPrefix::Else => write!(f, "else"),
            LabelPrefix::End => write!(f, "end"),
            LabelPrefix::Break => write!(f, "break"),
            LabelPrefix::Continue => write!(f, "continue"),
        }
    }
}

impl LabelPrefix {
    pub fn emit_label_identifier(self) -> String {
        use once_cell::sync::Lazy;
        static MAP: Lazy<std::sync::Mutex<HashMap<String, usize>>> =
            Lazy::new(|| std::sync::Mutex::new(HashMap::new()));
        let mut map = MAP.lock().unwrap();
        let count = map.entry(self.to_string()).or_insert(0);
        let label = format!("{}_{}", self, *count);
        *count += 1;
        label
    }
}

fn convert_loop_label(statement: &ast::AstNode) -> String {
    match statement {
        ast::AstNode::Break { id } => format!("break_{}", id),
        ast::AstNode::Continue { id } => format!("continue_{}", id),
        _ => {
            panic!("{:?} can't be label!", statement);
        }
    }
}

pub struct Tackilizer<'global> {
    pub variable_context: &'global mut VariableContext,
    pub symbol_table: &'global mut HashMap<String, symbol::SymbolInfo>,
}

impl<'global> Tackilizer<'global> {
    pub fn new(variable_context: &'global mut VariableContext, symbol_table: &'global mut HashMap<String, symbol::SymbolInfo>,) -> Self {
        Tackilizer { variable_context, symbol_table}
    }

    pub fn emit_program(&mut self, p: &ast::AstNode) -> Program {

        let ast::AstNode::Program { declarations } = p else { panic!("Expected Program node") };

        let mut top_levels = Vec::<TopLevel>::new();

        for top_level in declarations {

            match top_level {
                ast::AstNode::FunctionDecl { .. } => {
                    let function = self.emit_function(top_level);
                    if function.body.len() != 0 {
                        top_levels.push(TopLevel::Function(function));
                    }
                }
                ast::AstNode::Declaration { .. } => {
                    
                }, 
                _ => {
                    panic!("");
                }
            }

        }

        let tacky_defs = self.convert_symbols_to_tacky();
        let mut static_var_vec = Vec::<TopLevel>::new();
        for static_var in tacky_defs {
            static_var_vec.push(TopLevel::StaticVariable(static_var));
        }
        top_levels.extend(static_var_vec);


        return Program { top_levels: top_levels };
    }


    fn convert_symbols_to_tacky(&self) -> Vec<StaticVariable> {
        let mut tacky_defs = Vec::<StaticVariable>::new();

       for (name, symbol_info) in self.symbol_table.iter() {
        match &symbol_info.attrs {
            symbol::IdentifierAttrs::StaticAttr { init, global } => {
                match init {
                    symbol::InitialValue::Initial( init_val ) => {
                        tacky_defs.push(StaticVariable { name: name.clone(), global: global.clone(), init: init_val.clone() });
                    }, 

                    symbol::InitialValue::Tentative => {
                        tacky_defs.push(StaticVariable { name: name.clone(), global: global.clone(), init: 0 });
                    }

                    symbol::InitialValue::NoInitializer => continue
                }
            }
            _ => continue
            }
        }

        return tacky_defs;
    }

    fn emit_function(&mut self, f: &ast::AstNode) -> Function {
        match f {
            ast::AstNode::FunctionDecl { name, params, body , storage_class} => {
                let name = name.clone();

                let mut instructions = Vec::<Instruction>::new();

                if let ast::AstNode::Compound { block } = body.as_ref() {
                    for block_item in block {
                        self.emit_tacky(&block_item, &mut instructions);
                    }
                }

                Function {
                    name: name,
                    params: params.clone(),
                    body: instructions,
                    global: false,
                }
            }
            _ => {
                panic!("");
            }
        }
    }

    fn emit_tacky(&mut self, e: &ast::AstNode, instructions: &mut Vec<Instruction>) -> Operand {
        match e {
            ast::AstNode::Return { exp } => {
                let exp = Instruction::Return(self.emit_tacky(exp, instructions));
                instructions.push(exp);
                return Operand::Nothing;
            }
            ast::AstNode::Constant { value } => {
                return Operand::Constant(value.clone());
            }
            ast::AstNode::Unary {
                unary_operator,
                exp,
            } => {
                let src = self.emit_tacky(exp, instructions);
                let dst_name = self.variable_context.make_temporary(None);
                let dst = Operand::Var(dst_name);

                let tacky_op = convert_operator(unary_operator);

                instructions.push(Instruction::Unary(tacky_op, src, dst.clone()));
                return dst;
            }
            ast::AstNode::Binary {
                binary_operator,
                left,
                right,
            } => match binary_operator.as_ref() {
                ast::AstNode::LogicalAnd => {
                    let v1 = self.emit_tacky(left, instructions);

                    let label_false = LabelPrefix::False.emit_label_identifier();
                    let label_true = LabelPrefix::True.emit_label_identifier();

                    instructions.push(Instruction::JumpIfZero(v1, label_false.clone()));

                    let v2 = self.emit_tacky(right, instructions);

                    instructions.push(Instruction::JumpIfZero(v2, label_false.clone()));

                    let dst_name = self.variable_context.make_temporary(None);
                    let dst = Operand::Var(dst_name);

                    instructions.push(Instruction::Copy(Operand::Constant(1), dst.clone()));

                    instructions.push(Instruction::Jump(label_true.clone()));

                    instructions.push(Instruction::Label(label_false));

                    instructions.push(Instruction::Copy(Operand::Constant(0), dst.clone()));

                    instructions.push(Instruction::Label(label_true));

                    return dst;
                }
                ast::AstNode::LogicalOr => {
                    let v1 = self.emit_tacky(left, instructions);

                    let label_false = LabelPrefix::False.emit_label_identifier();
                    let label_true = LabelPrefix::True.emit_label_identifier();

                    instructions.push(Instruction::JumpIfNotZero(v1, label_true.clone()));

                    let v2 = self.emit_tacky(right, instructions);

                    instructions.push(Instruction::JumpIfNotZero(v2, label_true.clone()));

                    let dst_name = self.variable_context.make_temporary(None);
                    let dst = Operand::Var(dst_name);

                    instructions.push(Instruction::Copy(Operand::Constant(0), dst.clone()));

                    instructions.push(Instruction::Jump(label_false.clone()));

                    instructions.push(Instruction::Label(label_true));

                    instructions.push(Instruction::Copy(Operand::Constant(1), dst.clone()));

                    instructions.push(Instruction::Label(label_false));

                    return dst;
                }

                _ => {
                    let l = self.emit_tacky(left, instructions);
                    let r = self.emit_tacky(right, instructions);

                    let dst_name = self.variable_context.make_temporary(None);
                    let dst = Operand::Var(dst_name);

                    let op = convert_operator(&binary_operator);

                    instructions.push(Instruction::Binary(op, l, r, dst.clone()));
                    return dst;
                }
            },

            ast::AstNode::Var { identifier } => {
                return Operand::Var(identifier.to_string());
            }

            ast::AstNode::Assignment { exp1, exp2 } => {
                let var = self.emit_tacky(exp1, instructions);

                let result = self.emit_tacky(exp2, instructions);

                instructions.push(Instruction::Copy(result, var.clone()));
                return var;
            }

            ast::AstNode::Declaration { name, init , storage_class} => {
                if matches!(init.as_ref(), ast::AstNode::NULL) {
                    return Operand::Nothing;
                }

                let result = self.emit_tacky(init, instructions);
                instructions.push(Instruction::Copy(
                    result.clone(),
                    Operand::Var(name.to_string()),
                ));
                return result;
            }

            ast::AstNode::Expression { exp } => {
                return self.emit_tacky(exp, instructions);
            }

            ast::AstNode::If {
                condition,
                then,
                else_maybe,
            } => {
                let cond = self.emit_tacky(condition, instructions);

                let end_label = LabelPrefix::End.emit_label_identifier();

                if matches!(else_maybe.as_ref(), ast::AstNode::NULL) {
                    instructions.push(Instruction::JumpIfZero(cond, end_label.clone()));

                    self.emit_tacky(then, instructions);

                    instructions.push(Instruction::Label(end_label));
                } else {
                    let else_label = LabelPrefix::Else.emit_label_identifier();

                    instructions.push(Instruction::JumpIfZero(cond, else_label.clone()));

                    self.emit_tacky(then, instructions);

                    instructions.push(Instruction::Jump(end_label.clone()));

                    instructions.push(Instruction::Label(else_label.clone()));

                    self.emit_tacky(else_maybe, instructions);

                    instructions.push(Instruction::Label(end_label));
                }

                return Operand::Nothing;
            }

            ast::AstNode::Conditional {
                condition,
                if_exp,
                else_exp,
            } => {
                let cond = self.emit_tacky(condition, instructions);

                let else_label = LabelPrefix::Else.emit_label_identifier();
                let end_label = LabelPrefix::End.emit_label_identifier();

                let result = Operand::Var(self.variable_context.make_temporary(None));

                instructions.push(Instruction::JumpIfZero(cond, else_label.clone()));

                let if_result = self.emit_tacky(if_exp, instructions);

                instructions.push(Instruction::Copy(if_result, result.clone()));

                instructions.push(Instruction::Jump(end_label.clone()));

                instructions.push(Instruction::Label(else_label.clone()));

                let else_result = self.emit_tacky(else_exp, instructions);

                instructions.push(Instruction::Copy(else_result, result.clone()));

                instructions.push(Instruction::Label(end_label.clone()));

                return result;
            }

            ast::AstNode::For {
                init,
                condition,
                step,
                body,
                id,
            } => {
                let break_label_str = format!("break_{}", id);
                let continue_label_str = format!("continue_{}", id);

                self.emit_tacky(init, instructions);

                instructions.push(Instruction::Label(continue_label_str.clone()));

                let condition_result = self.emit_tacky(condition, instructions);

                if !matches!(condition_result, Operand::Nothing) {
                    instructions.push(Instruction::JumpIfZero(
                        condition_result,
                        break_label_str.clone(),
                    ));
                }

                self.emit_tacky(body, instructions);

                self.emit_tacky(step, instructions);

                instructions.push(Instruction::Jump(continue_label_str));

                instructions.push(Instruction::Label(break_label_str));

                return Operand::Nothing;
            }

            ast::AstNode::While {
                condition,
                body,
                id,
            } => {
                let break_label_str = format!("break_{}", id);
                let continue_label_str = format!("continue_{}", id);

                instructions.push(Instruction::Label(continue_label_str.clone()));

                let condition_result = self.emit_tacky(condition, instructions);

                instructions.push(Instruction::JumpIfZero(
                    condition_result,
                    break_label_str.clone(),
                ));

                self.emit_tacky(body, instructions);

                instructions.push(Instruction::Jump(continue_label_str));

                instructions.push(Instruction::Label(break_label_str));

                return Operand::Nothing;
            }

            ast::AstNode::DoWhile {
                body,
                condition,
                id,
            } => {
                let break_label_str = format!("break_{}", id);
                let continue_label_str = format!("continue_{}", id);

                instructions.push(Instruction::Label(continue_label_str.clone()));

                self.emit_tacky(body, instructions);

                let condition_result = self.emit_tacky(condition, instructions);

                instructions.push(Instruction::JumpIfZero(
                    condition_result,
                    break_label_str.clone(),
                ));

                instructions.push(Instruction::Jump(continue_label_str));

                instructions.push(Instruction::Label(break_label_str));

                return Operand::Nothing;
            }

            ast::AstNode::Break { id } => {
                instructions.push(Instruction::Jump(format!("break_{}", id)));
                return Operand::Nothing;
            }

            ast::AstNode::Continue { id } => {
                instructions.push(Instruction::Jump(format!("continue_{}", id)));
                return Operand::Nothing;
            }

            ast::AstNode::FunctionCall { identifier, args } => {
                let mut args_result = Vec::<Operand>::new();
                for arg in args {
                    args_result.push(self.emit_tacky(arg, instructions));
                }

                let dst = Operand::Var(self.variable_context.make_temporary(None));
                let result = Instruction::FunCall(identifier.clone(), args_result, dst.clone());
                instructions.push(result);

                return dst;
            }

            ast::AstNode::Compound { block } => {
                for block_item in block {
                    self.emit_tacky(block_item, instructions);
                }

                return Operand::Nothing;
            }

            ast::AstNode::NULL => {
                return Operand::Nothing;
            }

            ast::AstNode::FunctionDecl { .. } => Operand::Nothing,

            _ => {
                panic!("Unimplement tacky {:?}", e);
            }
        }
    }
}

fn convert_operator(op: &ast::AstNode) -> Operator {
    match op {
        ast::AstNode::Negate => Operator::Negate,
        ast::AstNode::Complement => Operator::Complement,
        ast::AstNode::LogicalNot => Operator::Not,
        ast::AstNode::Add => Operator::Add,
        ast::AstNode::Sub => Operator::Sub,
        ast::AstNode::Mul => Operator::Mul,
        ast::AstNode::Div => Operator::Div,
        ast::AstNode::Mod => Operator::Remainder,
        ast::AstNode::GreaterOrEqual => Operator::GreaterOrEqual,
        ast::AstNode::GreaterThan => Operator::GreaterThan,
        ast::AstNode::LessOrEqual => Operator::LessOrEqual,
        ast::AstNode::LessThan => Operator::LessThan,
        ast::AstNode::Equal => Operator::Equal,
        ast::AstNode::NotEqual => Operator::NotEqual,

        _ => {
            eprintln!("Unimplement tacky operator {:?}", op);
            panic!("");
        }
    }
}
