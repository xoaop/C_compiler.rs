use core::str;
use crate::ast;

#[derive(Debug)]
pub struct AssemblyAst {
    program: Box<Program>,
}
#[derive(Debug)]
pub struct Program {
    function_definition: Box<Function>,
}
#[derive(Debug)]
pub struct Function {
    identifier: Identifier, 
    instructions: Vec<Instruction>,
}
#[derive(Debug)]
pub struct Identifier {
    name: String,
}
#[derive(Debug)]
enum Instruction {
    Mov(Operand, Operand), 
    Ret,
}
#[derive(Debug)]
enum Operand {
    Imm(i64), 
    Register,
}

fn trans_operand(exp: &ast::AstNode) -> Operand {
    match exp {
        ast::AstNode::Constant { value } => {
            return Operand::Imm(*value);
        }
        _ => {
            eprintln!("Debug: The node {:#?} is not a Constant variant.", exp);
            std::process::exit(1);
        }
    }
}

fn trans_identifier(identifier: &ast::AstNode) -> Identifier {
    match identifier {
        ast::AstNode::Identifier(str) => Identifier { name: str.clone() },
        _ => {
            eprintln!("Debug: The node {:#?} is not an Identifier variant.", identifier);
            std::process::exit(1);
        }
    }
}

fn trans_instructions(body: &ast::AstNode) ->Vec<Instruction> {
    let mut instructions = Vec::<Instruction>::new();
    match body {
        ast::AstNode::Return { exp } => {
            instructions.push(Instruction::Mov(trans_operand(exp), Operand::Register));
            instructions.push(Instruction::Ret);
        }

        _ => {
            eprintln!("Debug: The node {:#?} is not a Return variant.", body);
            std::process::exit(1);
        }
    }
    return instructions;
}


fn trans_function(function: &ast::AstNode) -> Function{
    match function {
        ast::AstNode::Function { name, body } => {
            let identifier = trans_identifier(name);
            let instructions = trans_instructions(body);

            return Function {identifier: identifier, instructions: instructions};
        }
        _ => {
            eprintln!("Debug: The node {:#?} is not a Function variant.", function);
            std::process::exit(1);
        }
    }
}

fn trans_program(program: &ast::AstNode) -> Program {
    match program {
        ast::AstNode::Program { function_definition } => {
            let function = trans_function(function_definition);
            return Program { function_definition: Box::new(function) };
        }

        _ => {
            eprintln!("Debug: The node {:#?} is not a Program variant.", program);
            std::process::exit(1);
        }
    }
}


pub fn trans_ast_to_asmast(ast: &ast::Ast) -> AssemblyAst {
    return AssemblyAst { program: Box::new(trans_program(&*ast.root)) };
}