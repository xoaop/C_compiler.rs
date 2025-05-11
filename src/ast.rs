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
        value: i64,            // 整数值（int）
    },
    Identifier(String), 
}

#[derive(Debug)]
pub struct Ast {
    pub root: Box<AstNode>,
}





