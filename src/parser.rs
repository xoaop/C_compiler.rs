use crate::ast::{AstNode, Ast};
use crate::lexer::{Token, TokenList, TokenType};
use std::mem;


pub struct Parser {
    token_list: TokenList,
}

impl Parser {

    pub fn new(token_list: TokenList) -> Self {
        Parser { token_list }
    }

    fn expert(&mut self, expected: &TokenType) -> Option<&Token> {
        let actual = self.token_list.next_token();

        if mem::discriminant(&actual.unwrap().token_type) != mem::discriminant(expected) {
            eprintln!("语法错误！");
            std::process::exit(1);
        }

        return actual;
    }

    fn parse_exp(&mut self) -> AstNode {
        let exp_token = self.expert(&TokenType::Integer(0)).unwrap();

        let new_exp_node = AstNode::Constant { value: *exp_token.token_type.as_value::<i64>().unwrap()};

        return new_exp_node;

    }

    fn parse_statement(&mut self) ->Box<AstNode> {

        self.expert(&TokenType::KeywordReturn);

        let return_val = self.parse_exp();

        self.expert(&TokenType::Semicolon);

        return Box::new( AstNode::Return { exp: Box::new(return_val) } );
    }

    fn parse_function(&mut self) -> Box<AstNode> {
        self.expert(&TokenType::KeywordInt);

        let identifier = self.expert(&TokenType::Identifier("".to_string())).unwrap().token_type.as_value::<String>().unwrap().clone();

        self.expert(&TokenType::LelfBracket);
        self.expert(&TokenType::RightBracket);

        self.expert(&TokenType::LcurlyBracket);

        let statement = self.parse_statement();

        self.expert(&TokenType::RcurlyBracket);

        return Box::new(AstNode::Function { name: Box::new( AstNode::Identifier(identifier) ), body: statement });
    }

    fn parse_program(&mut self) -> Box<AstNode> {
        let function = self.parse_function();

        return Box::new( AstNode::Program { function_definition: function } );
    }

    pub fn parse(&mut self) -> Ast {

        return Ast { root: self.parse_program() };
    }


}