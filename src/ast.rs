#[derive(Debug)]
pub enum AstNode {
    Program {
        function_definition: Box<AstNode>,
    },
    Function {
        name: Box<AstNode>,    // 函数名（identifier）
        body: Box<AstNode>,    // 函数体（statement）
    },
    Return {
        exp: Box<AstNode>,     // 返回值（表达式）
    },
    Constant {
        value: i32,            // 整数值（int）
    },
    Identifier(String), 

    // Unary operators:
    Unary {
        unary_operator: Box<AstNode>,
        exp: Box<AstNode>,
    },

    Complement, // Bitwise NOT operator (~)
    Negate, // Negation operator (-)

    //binary operators:
    Binary {
        binary_operator: Box<AstNode>,
        left: Box<AstNode>,
        right:Box<AstNode>,
    },

    Add,
    Sub,
    Mul,
    Div,
    Mod,


    //others:

}

#[derive(Debug)]
pub struct Ast {
    pub root: Box<AstNode>,
}





