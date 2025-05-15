

use crate::ast;

#[derive(Debug)]
pub enum Instruction {
    Return(Operand),
    Unary(Operator, Operand, Operand),
}

#[derive(Debug)]
pub enum Operator {
    Negate, 
    Complement,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Constant(i32), 
    Var(String),
}

#[derive(Debug)]
pub struct Program {
    pub function_def: Function,
}

#[derive(Debug)]
pub struct Function {
   pub name: String, 
   pub body: Vec<Instruction>,
}


pub fn emit_program(p: &ast::AstNode) -> Program {
    match p {
        ast::AstNode::Program { function_definition } => {
            Program { function_def: emit_function(function_definition)}
        }
        _ => {
            std::process::exit(1);
        }
    }
}

fn emit_function(f: &ast::AstNode) -> Function {
    match f {
        ast::AstNode::Function { name, body } => {
            let name_ = &**name;

            let name = match name_ {
                ast::AstNode::Identifier(name) => name.clone(),
                _ => {
                    std::process::exit(1);
                }
            };

            let body_ = &**body;

            let mut instructions = Vec::<Instruction>::new();
            
            let body = match body_ {
                ast::AstNode::Return { exp } => {
                    let exp = Instruction::Return(emit_tacky(&exp, &mut instructions));
                    instructions.push(exp);
                }
                _ => {
                    std::process::exit(1);
                }
            };

            Function { name: name, body: instructions }

        },
        _ => {
            std::process::exit(1);
        }
    }
}

fn emit_tacky(e: &ast::AstNode, instructions: &mut Vec<Instruction>) -> Operand {
    match e {
        ast::AstNode::Constant { value } => {
            return Operand::Constant(value.clone());
        },
        ast::AstNode::Unary { unary_operator, exp } => {
            let src = emit_tacky(exp, instructions);
            let dst_name = make_temporary();
            let dst = Operand::Var(dst_name);

            let tacky_op = convert_operator(unary_operator);

            instructions.push(Instruction::Unary(tacky_op, src, dst.clone()));
            return dst;
        }
        _ => {
            eprintln!("Unimplement tacky!");
            std::process::exit(1);
        }
    }
} 

fn make_temporary() -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static IDX: AtomicUsize = AtomicUsize::new(0);
    let current_idx = IDX.fetch_add(1, Ordering::SeqCst);
    format!("tmp.{}", current_idx)
}

fn convert_operator(op: &ast::AstNode) -> Operator {
    match op {
        ast::AstNode::Negate => { Operator::Negate },
        ast::AstNode::Complement => { Operator::Complement },
        _ => {
            eprintln!("Unimplement tacky operator!");
            std::process::exit(1);
        }
    }
}