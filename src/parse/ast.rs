

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Program {
        declarations: Vec<AstNode>,
    },

    //storage_class
    Static, 
    Extern,

    //Type
    TypeInt,

    //statement:
    Return {
        exp: Box<AstNode>, // 返回值（表达式）
    },
    Expression {
        exp: Box<AstNode>,
    },
    If {
        condition: Box<AstNode>,
        then: Box<AstNode>,
        else_maybe: Box<AstNode>,
    },
    Compound {
        block: Vec<AstNode>, // 复合语句（块）
    },
    Break {
        id: i32,
    },
    Continue {
        id: i32,
    },
    While {
        condition: Box<AstNode>,
        body: Box<AstNode>,
        id: i32,
    }, 
    DoWhile {
        body: Box<AstNode>,
        condition: Box<AstNode>,
        id: i32,
    }, 
    For {
        init: Box<AstNode>,
        condition: Box<AstNode>,
        step: Box<AstNode>,
        body: Box<AstNode>,
        id: i32,
    },
    NULL,

    //variable declaration
    Declaration {
        name: String,
        init: Box<AstNode>,
        storage_class: Box<AstNode>,
    },

    FunctionDecl {
        name: String, 
        params: Vec<String>,
        body: Box<AstNode>, // optional
        storage_class: Box<AstNode>,
    },

    // Identifier(String),

    //exp:
    Constant {
        value: i32, // 整数值（int）
    },
    Var {
        identifier: String,
    },
    // Unary operators:
    Unary {
        unary_operator: Box<AstNode>,
        exp: Box<AstNode>,
    },
    Assignment {
        exp1: Box<AstNode>,
        exp2: Box<AstNode>,
    },
    Conditional {
        condition: Box<AstNode>,
        if_exp: Box<AstNode>,
        else_exp: Box<AstNode>,
    },

    FunctionCall {
        identifier: String, 
        args: Vec<AstNode>,
    },

    //binary operators:
    Binary {
        binary_operator: Box<AstNode>,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },

    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    Increment,      // ++
    Decrement,      // --
    LogicalNot,     // !
    LogicalAnd,     // &&
    LogicalOr,      // ||
    Equal,          // ==
    NotEqual,       // !=
    GreaterThan,    // >
    GreaterOrEqual, // >=
    LessThan,       // <
    LessOrEqual,    // <=

    Complement, // Bitwise NOT operator (~)
    Negate,     // Negation operator (-)
}



#[derive(Debug)]
pub struct Ast {
    pub root: Box<AstNode>,
}
