use crate::ast;
use crate::lexer;
use crate::lexer::get_token_list;
use std::mem;


pub struct Parser {
    token_list: lexer::TokenList,
}

impl Parser {

    pub fn new(token_list: lexer::TokenList) -> Self {
        Parser { token_list }
    }


    fn expert(&mut self, expected: &lexer::TokenType) -> &lexer::Token {
        let actual = self.token_list.next_token().unwrap();

        if mem::discriminant(&actual.token_type) != mem::discriminant(expected) {
            eprintln!("语法错误！expert: {:?}, actual: {:?}", expected, actual.token_type);
            std::process::exit(1);
        }

        return actual;
    }

    fn parse_binop(token: &lexer::TokenType) -> ast::AstNode {
        match token {
            lexer::TokenType::Plus => ast::AstNode::Add,
            lexer::TokenType::Hyphen => ast::AstNode::Sub,
            lexer::TokenType::Asterisk => ast::AstNode::Mul,
            lexer::TokenType::ForwardSlash => ast::AstNode::Div,
            lexer::TokenType::Percent => ast::AstNode::Mod,

            _ => {
                std::panic!(
                    "Unsupported binary operator: {:?}", token
                )
            }
        }
    }

    fn precedence(&self, operator: &lexer::TokenType) -> i32 {
        match operator {
            lexer::TokenType::Plus | lexer::TokenType::Hyphen => 45,
            lexer::TokenType::Asterisk | lexer::TokenType::ForwardSlash => 50,
            _ => { std::panic!("unvaild operator!") } 
        }
    }

    fn parse_exp(&mut self, min_prec: i32) -> ast::AstNode {
        let mut left = self.parse_factor();

        while let Some(next_token) = self.token_list.next_token() {
            let token_type = next_token.token_type.clone();

            if token_type.is_binary_operator() && self.precedence(&token_type) > min_prec {
                let prec = self.precedence(&token_type);

                let operator = Self::parse_binop(&token_type);
                let right = self.parse_exp(prec);
                left = ast::AstNode::Binary { binary_operator: Box::new(operator), left: Box::new(left), right: Box::new(right) };
            }
            else {
                self.token_list.back();
                break;
            }
        }

        left
    }

    fn parse_factor(&mut self) -> ast::AstNode {
        let next_token = self.token_list.next_token().expect("Tokenlist is at the end!");

        match next_token.token_type {
            lexer::TokenType::Integer(i32) => {
                return ast::AstNode::Constant { value: i32 };
            }
            lexer::TokenType::Tilde => {
                let operator = ast::AstNode::Complement;
                let inner_exp = self.parse_factor();
                return ast::AstNode::Unary { unary_operator: Box::new(operator), exp: Box::new(inner_exp) };
            }
            lexer::TokenType::Hyphen => {
                let operator = ast::AstNode::Negate;
                let inner_exp = self.parse_factor();
                return ast::AstNode::Unary { unary_operator: Box::new(operator), exp: Box::new(inner_exp) };
            }
            lexer::TokenType::LelfBracket => {
                let inner_exp = self.parse_exp(0);
                self.expert(&lexer::TokenType::RightBracket);
                return inner_exp;
            }
            _ => {
                eprintln!("Unexpected token: {:?}", next_token.token_type);
                std::process::exit(1);
            }
        }
    }
 
    fn parse_statement(&mut self) ->Box<ast::AstNode> {

        self.expert(&lexer::TokenType::KeywordReturn);

        let return_val = self.parse_exp(0);

        self.expert(&lexer::TokenType::Semicolon);

        return Box::new( ast::AstNode::Return { exp: Box::new(return_val) } );
    }

    fn parse_function(&mut self) -> Box<ast::AstNode> {
        self.expert(&lexer::TokenType::KeywordInt);

        let identifier_token = &self.expert(&lexer::TokenType::Identifier("".to_string()));

        let identifier = match &identifier_token.token_type {
            lexer::TokenType::Identifier(name) => name.clone(),
            _ => {
                        std::process::exit(1);
            }
        };

        self.expert(&lexer::TokenType::LelfBracket);
        self.expert(&lexer::TokenType::RightBracket);

        self.expert(&lexer::TokenType::LcurlyBracket);

        let statement = self.parse_statement();

        self.expert(&lexer::TokenType::RcurlyBracket);

        return Box::new(ast::AstNode::Function { name: Box::new( ast::AstNode::Identifier(identifier) ), body: statement });
    }

    fn parse_program(&mut self) -> Box<ast::AstNode> {
        let function = self.parse_function();

        return Box::new( ast::AstNode::Program { function_definition: function } );
    }

    pub fn parse(&mut self) -> ast::Ast {

        return ast::Ast { root: self.parse_program() };
    }


}