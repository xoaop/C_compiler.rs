use crate::global::VariableContext;
use crate::lex::token::{Token, TokenList, TokenType};
use crate::parse::ast;

use core::panic;
use std::collections::HashMap;
use std::mem;

#[derive(Clone)]
pub struct MapEntry {
    unique_name: String,
    from_current_scope: bool,
    has_linkage: bool,
}

pub enum Type {
    Int,
    FunType { param_count: i32, defined: bool },
}

pub struct Parser<'variable_context> {
    token_list: TokenList,
    pub variable_context: &'variable_context mut VariableContext,
    pub symbol_table: &'variable_context mut HashMap<String, Type>,
}

impl<'variable_context> Parser<'variable_context> {
    pub fn new(
        token_list: TokenList,
        variable_context: &'variable_context mut VariableContext,
        symbol_table: &'variable_context mut HashMap<String, Type>,
    ) -> Self {
        Parser {
            token_list,
            variable_context,
            symbol_table,
        }
    }

    fn expect(&mut self, expected: &TokenType) -> &Token {
        let actual = self.token_list.next_token().unwrap();
        if mem::discriminant(&actual.token_type) != mem::discriminant(expected) {
            eprintln!(
                "语法错误！expect: {:?}, actual: {:?}",
                expected, actual.token_type
            );
            std::process::exit(1);
        }
        actual
    }

    pub fn parse(&mut self) -> ast::Ast {
        ast::Ast {
            root: Box::new(self.parse_program()),
        }
    }

    fn parse_unray_operator(token: &TokenType) -> ast::AstNode {
        match token {
            TokenType::Exclamation => ast::AstNode::LogicalNot,
            TokenType::TwoHyphen => ast::AstNode::Decrement,
            TokenType::TwoPlus => ast::AstNode::Increment,
            TokenType::Tilde => ast::AstNode::Complement,
            TokenType::Hyphen => ast::AstNode::Negate,
            _ => {
                std::panic!("Unsupported operator: {:?}", token)
            }
        }
    }

    fn parse_operator(token: &TokenType) -> ast::AstNode {
        match token {
            TokenType::Exclamation => ast::AstNode::LogicalNot,
            TokenType::TwoAnd => ast::AstNode::LogicalAnd,
            TokenType::TwoOr => ast::AstNode::LogicalOr,
            TokenType::TwoEqual => ast::AstNode::Equal,
            TokenType::NotEqual => ast::AstNode::NotEqual,
            TokenType::Less => ast::AstNode::LessThan,
            TokenType::LessEqual => ast::AstNode::LessOrEqual,
            TokenType::Greater => ast::AstNode::GreaterThan,
            TokenType::GreaterEqual => ast::AstNode::GreaterOrEqual,
            TokenType::TwoPlus => ast::AstNode::Increment,
            TokenType::TwoHyphen => ast::AstNode::Decrement,
            TokenType::Tilde => ast::AstNode::Complement,

            TokenType::Plus => ast::AstNode::Add,
            TokenType::Hyphen => ast::AstNode::Sub,
            TokenType::Asterisk => ast::AstNode::Mul,
            TokenType::ForwardSlash => ast::AstNode::Div,
            TokenType::Percent => ast::AstNode::Mod,

            _ => {
                std::panic!("Unsupported operator: {:?}", token)
            }
        }
    }

    fn precedence(&self, operator: &TokenType) -> i32 {
        match operator {
            TokenType::TwoPlus | TokenType::TwoHyphen => 100,
            TokenType::Asterisk | TokenType::ForwardSlash | TokenType::Percent => 50,
            TokenType::Plus | TokenType::Hyphen => 45,
            TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual => 35,
            TokenType::TwoEqual | TokenType::NotEqual => 30,
            TokenType::TwoAnd => 10,
            TokenType::TwoOr => 5,
            TokenType::QuestionMark => 3,
            TokenType::Equal => 1,

            _ => std::panic!("unvaild operator!"),
        }
    }

    fn parse_exp(&mut self, min_prec: i32) -> ast::AstNode {
        let mut left = self.parse_factor();
        while let Some(next_token) = self.token_list.next_token() {
            let token_type = next_token.token_type.clone();
            if token_type.is_binary_operator() && self.precedence(&token_type) >= min_prec {
                let prec = self.precedence(&token_type);

                if matches!(token_type, TokenType::Equal) {
                    let right = self.parse_exp(prec);
                    left = ast::AstNode::Assignment {
                        exp1: Box::new(left),
                        exp2: Box::new(right),
                    };
                } else if matches!(token_type, TokenType::QuestionMark) {
                    let middle = self.parse_exp(0);

                    self.expect(&TokenType::Colon);

                    let right = self.parse_exp(prec);

                    left = ast::AstNode::Conditional {
                        condition: Box::new(left),
                        if_exp: Box::new(middle),
                        else_exp: Box::new(right),
                    };
                } else if matches!(token_type, TokenType::TwoPlus | TokenType::TwoHyphen) {
                    let operator = Self::parse_unray_operator(&token_type);
                    left = ast::AstNode::Unary {
                        unary_operator: Box::new(operator),
                        exp: Box::new(left),
                    };
                } else {
                    let operator = Self::parse_operator(&token_type);
                    let right = self.parse_exp(prec + 1);
                    left = ast::AstNode::Binary {
                        binary_operator: Box::new(operator),
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                }
            } else if matches!(token_type, TokenType::TwoPlus)
                || matches!(token_type, TokenType::TwoHyphen)
            // 右++和右--
            {
                let operator = Self::parse_operator(&token_type);
                left = ast::AstNode::Unary {
                    unary_operator: Box::new(operator),
                    exp: Box::new(left),
                };
            } else {
                self.token_list.back();
                break;
            }
        }
        return left;
    }

    fn parse_factor(&mut self) -> ast::AstNode {
        let next_token = self
            .token_list
            .next_token()
            .expect("Tokenlist is at the end!");
        match next_token.token_type.clone() {
            //标量
            TokenType::Integer(i32) => ast::AstNode::Constant { value: i32 },

            //单元运算符
            TokenType::Tilde
            | TokenType::Hyphen
            | TokenType::Exclamation
            | TokenType::TwoHyphen
            | TokenType::TwoPlus => {
                let operator = Self::parse_unray_operator(&next_token.token_type);
                let inner_exp = self.parse_factor();
                ast::AstNode::Unary {
                    unary_operator: Box::new(operator),
                    exp: Box::new(inner_exp),
                }
            }

            //(表达式)
            TokenType::LelfBracket => {
                let inner_exp = self.parse_exp(0);
                self.expect(&TokenType::RightBracket);
                inner_exp
            }

            TokenType::Identifier(ident) => {
                if matches!(
                    self.token_list.current_token().unwrap().token_type,
                    TokenType::LelfBracket
                ) {
                    self.expect(&TokenType::LelfBracket);

                    let mut args = Vec::<ast::AstNode>::new();
                    while !matches!(
                        self.token_list.current_token().unwrap().token_type,
                        TokenType::RightBracket
                    ) {
                        args.push(self.parse_exp(0));

                        if !matches!(
                            self.token_list.current_token().unwrap().token_type,
                            TokenType::RightBracket
                        ) {
                            self.expect(&TokenType::Comma);
                        }
                    }

                    self.expect(&TokenType::RightBracket);

                    return ast::AstNode::FunctionCall {
                        identifier: ident,
                        args: args,
                    };
                } else {
                    return ast::AstNode::Var { identifier: ident };
                }
            }

            _ => {
                panic!(
                    "Unexpected token: {}:{:?} ",
                    next_token.index, next_token.token_type
                );
            }
        }
    }

    fn parse_statement(&mut self) -> ast::AstNode {
        let current_token = self.token_list.current_token().unwrap();

        match current_token.token_type.clone() {
            TokenType::KeywordReturn => {
                self.expect(&TokenType::KeywordReturn);
                let return_val = self.parse_exp(0);
                self.expect(&TokenType::Semicolon);
                return ast::AstNode::Return {
                    exp: Box::new(return_val),
                };
            }
            TokenType::KeywordIf => {
                self.expect(&TokenType::KeywordIf);
                self.expect(&TokenType::LelfBracket);
                let condition_exp = self.parse_exp(0);
                self.expect(&TokenType::RightBracket);
                let if_statement = self.parse_statement();

                let mut else_statement = ast::AstNode::NULL;
                if matches!(
                    self.token_list.current_token().unwrap().token_type,
                    TokenType::KeywordElse
                ) {
                    self.expect(&TokenType::KeywordElse);
                    else_statement = self.parse_statement();
                }

                return ast::AstNode::If {
                    condition: Box::new(condition_exp),
                    then: Box::new(if_statement),
                    else_maybe: Box::new(else_statement),
                };
            }

            TokenType::KeywordFor => {
                self.expect(&TokenType::KeywordFor);

                self.expect(&TokenType::LelfBracket);

                let mut init = ast::AstNode::NULL;
                if !matches!(
                    &self.token_list.current_token().unwrap().token_type,
                    &TokenType::Semicolon
                ) {
                    init = self.parse_block_item();
                } else {
                    self.expect(&TokenType::Semicolon);
                }

                let mut condition = ast::AstNode::NULL;
                if !matches!(
                    &self.token_list.current_token().unwrap().token_type,
                    &TokenType::Semicolon
                ) {
                    condition = self.parse_exp(0);
                }

                self.expect(&TokenType::Semicolon);

                let mut step = ast::AstNode::NULL;
                if !matches!(
                    &self.token_list.current_token().unwrap().token_type,
                    &TokenType::RightBracket
                ) {
                    step = self.parse_exp(0);
                }

                self.expect(&TokenType::RightBracket);

                let body = self.parse_statement();

                return ast::AstNode::For {
                    init: Box::new(init),
                    condition: Box::new(condition),
                    step: Box::new(step),
                    body: Box::new(body),
                    id: self.variable_context.record_loop_id(),
                };
            }

            TokenType::KeywordWhile => {
                self.expect(&TokenType::KeywordWhile);

                self.expect(&TokenType::LelfBracket);

                let condition = self.parse_exp(0);

                self.expect(&TokenType::RightBracket);

                let body = self.parse_statement();

                return ast::AstNode::While {
                    condition: Box::new(condition),
                    body: Box::new(body),
                    id: self.variable_context.record_loop_id(),
                };
            }

            //Do and While
            TokenType::KeywordDo => {
                self.expect(&TokenType::KeywordDo);

                let body = self.parse_statement();

                self.expect(&TokenType::KeywordWhile);

                self.expect(&TokenType::LelfBracket);

                let condition = self.parse_exp(0);

                self.expect(&TokenType::RightBracket);

                return ast::AstNode::DoWhile {
                    body: Box::new(body),
                    condition: Box::new(condition),
                    id: self.variable_context.record_loop_id(),
                };
            }

            TokenType::KeywordBreak => {
                self.expect(&TokenType::KeywordBreak);
                self.expect(&TokenType::Semicolon);

                return ast::AstNode::Break { id: -1 };
            }

            TokenType::KeywordContinue => {
                self.expect(&TokenType::KeywordContinue);
                self.expect(&TokenType::Semicolon);

                return ast::AstNode::Continue { id: -1 };
            }

            TokenType::LcurlyBracket => {
                return self.parse_compound();
            }

            TokenType::Semicolon => {
                self.expect(&TokenType::Semicolon);

                return ast::AstNode::NULL;
            }
            _ => {
                let exp = ast::AstNode::Expression {
                    exp: Box::new(self.parse_exp(0)),
                };
                self.expect(&TokenType::Semicolon);
                return exp;
            }
        }
    }

    fn parse_declaration(&mut self) -> ast::AstNode {
        self.expect(&TokenType::KeywordInt);

        let ident = self
            .expect(&TokenType::Identifier("".to_string()))
            .token_type
            .clone();

        let ident = match ident {
            TokenType::Identifier(ident) => ident,
            _ => {
                panic!("parser: 语法错误!");
            }
        };

        //函数声明
        if matches!(
            self.token_list.current_token().unwrap().token_type,
            TokenType::LelfBracket
        ) {
            let params = self.parse_params();

            self.expect(&TokenType::Semicolon);

            return ast::AstNode::FunctionDecl {
                name: ident,
                params: params,
                body: Box::new(ast::AstNode::NULL),
            };
        }

        //变量声明
        let mut exp = ast::AstNode::NULL;
        if matches!(
            self.token_list.current_token().unwrap().token_type,
            TokenType::Equal
        ) {
            self.expect(&TokenType::Equal);
            exp = self.parse_exp(0);
        }

        self.expect(&TokenType::Semicolon);

        return ast::AstNode::Declaration {
            name: ident,
            init: Box::new(exp),
        };
    }

    fn parse_block_item(&mut self) -> ast::AstNode {
        let current_token = self.token_list.current_token().unwrap();
        match current_token.token_type {
            //TODO: 添加对局部函数声明的解析
            TokenType::KeywordInt => self.parse_declaration(),

            _ => self.parse_statement(),
        }
    }

    fn parse_block_items(&mut self) -> Vec<ast::AstNode> {
        let mut block = Vec::<ast::AstNode>::new();
        while !matches!(
            self.token_list.current_token().unwrap().token_type,
            TokenType::RcurlyBracket
        ) {
            let block_item = self.parse_block_item();
            block.push(block_item);
        }

        return block;
    }

    fn parse_compound(&mut self) -> ast::AstNode {
        self.expect(&TokenType::LcurlyBracket);

        let block = self.parse_block_items();

        self.expect(&TokenType::RcurlyBracket);

        // let block = resolve::resolve_block(block, &mut HashMap::<String, resolve::MapEntry>::new());

        return ast::AstNode::Compound { block: block };
    }

    fn parse_params(&mut self) -> Vec<String> {
        let mut params = Vec::<String>::new();

        self.expect(&TokenType::LelfBracket);

        while !matches!(
            self.token_list.current_token().unwrap().token_type,
            TokenType::RightBracket
        ) {
            match self.token_list.current_token().unwrap().token_type {
                TokenType::KeywordVoid => {
                    self.expect(&TokenType::KeywordVoid);
                    break;
                }
                _ => {
                    self.expect(&TokenType::KeywordInt);
                }
            }

            if let TokenType::Identifier(ident) =
                self.token_list.current_token().unwrap().token_type.clone()
            {
                params.push(ident);
            }

            self.expect(&TokenType::Identifier("".to_string()));

            if !matches!(
                self.token_list.current_token().unwrap().token_type,
                TokenType::Comma
            ) {
                break;
            }
            self.expect(&TokenType::Comma);
        }

        self.expect(&TokenType::RightBracket);

        return params;
    }

    fn parse_function_decl(&mut self) -> ast::AstNode {
        self.expect(&TokenType::KeywordInt);
        let identifier_token = self.expect(&TokenType::Identifier("".to_string()));
        let identifier = match &identifier_token.token_type {
            TokenType::Identifier(name) => name.clone(),
            _ => std::process::exit(1),
        };

        let params = self.parse_params();

        let mut compound = ast::AstNode::NULL;

        if !matches!(
            self.token_list.current_token().unwrap().token_type,
            TokenType::Semicolon
        ) {
            compound = self.parse_compound();
        } else {
            self.expect(&TokenType::Semicolon);
        }

        return ast::AstNode::FunctionDecl {
            name: identifier,
            params: params,
            body: Box::new(compound),
        };
    }

    fn parse_program(&mut self) -> ast::AstNode {
        let mut funcs = Vec::<ast::AstNode>::new();

        let mut identifier_map = HashMap::<String, MapEntry>::new();

        while self.token_list.current_token().is_some() {
            let func = self.parse_function_decl();
            let func = self.resolve_function_declaration(func, &mut identifier_map);
            funcs.push(func);
        }

        return ast::AstNode::Program {
            function_decl: funcs,
        };
    }

    // =======================
    //   Resolve Part is Below
    // =======================

    fn resolve_function_declaration(
        &mut self,
        decl: ast::AstNode,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> ast::AstNode {
        match decl {
            ast::AstNode::FunctionDecl { name, params, body } => {
                if identifier_map.contains_key(&name) {
                    let prev_entry = identifier_map.get(&name).unwrap();

                    if prev_entry.from_current_scope && !prev_entry.has_linkage {
                        panic!(
                            "符号 '{}' 在当前作用域已被声明，且未具有链接属性，说明当前作用域已经有同名变量，不能再被当作函数声明",
                            name
                        );
                    }
                }

                identifier_map.insert(
                    name.clone(),
                    MapEntry {
                        unique_name: name.clone(),
                        from_current_scope: true,
                        has_linkage: true,
                    },
                );

                let mut inner_map = identifier_map.clone();

                //TODO: FIX, 如果没有定义(body为null), 不需要记录参数名? 不确定

                let mut new_params = Vec::<String>::new();
                let mut new_body = ast::AstNode::NULL;

                if !matches!(*body, ast::AstNode::NULL) {
                    // *有待检查
                    for param in params {
                        new_params.push(self.resolve_param(param, &mut inner_map));
                    }
                    new_body = self.resolve_compound(*body, None, &mut inner_map);
                } else {
                    new_params = params;
                }

                let decl = ast::AstNode::FunctionDecl {
                    name: name.clone(),
                    params: new_params,
                    body: Box::new(new_body),
                };

                self.type_check_func_decl(&decl);

                return decl;
            }

            _ => {
                panic!("");
            }
        }
    }

    fn resolve_param(
        &mut self,
        param: String,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> String {
        if identifier_map.contains_key(&param)
            && identifier_map.get(&param).unwrap().from_current_scope
        {
            panic!("Duplicate parameter declaration: {}", param);
        }

        let unique_name = self.variable_context.make_temporary(Some(param.clone()));
        identifier_map.insert(
            param.clone(),
            MapEntry {
                unique_name: unique_name.clone(),
                from_current_scope: true,
                has_linkage: false,
            },
        );

        return unique_name;
    }

    fn resolve_block_item(
        &mut self,
        block_item: ast::AstNode,
        current_loop_id: Option<i32>,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> ast::AstNode {
        let resolved = match block_item {
            ast::AstNode::Declaration { .. } => {
                self.resolve_declaration(&block_item, identifier_map)
            }
            ast::AstNode::FunctionDecl { .. } => {
                self.resolve_function_declaration(block_item, identifier_map)
            }
            _ => self.resolve_statement(&block_item, current_loop_id, identifier_map),
        };

        return resolved;
    }

    pub fn resolve_block(
        &mut self,
        block_items: Vec<ast::AstNode>,
        current_loop_id: Option<i32>,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> Vec<ast::AstNode> {
        let mut resolved_block_items = Vec::<ast::AstNode>::new();

        for block_item in block_items {
            let resolved = self.resolve_block_item(block_item, current_loop_id, identifier_map);
            resolved_block_items.push(resolved);
        }

        resolved_block_items
    }

    fn resolve_compound(
        &mut self,
        compound: ast::AstNode,
        current_loop_id: Option<i32>,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> ast::AstNode {
        if let ast::AstNode::Compound { block } = compound {
            return ast::AstNode::Compound {
                block: self.resolve_block(block, current_loop_id, identifier_map),
            };
        } else {
            panic!("");
        }
    }

    fn resolve_declaration(
        &mut self,
        declaration: &ast::AstNode,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> ast::AstNode {
        if let ast::AstNode::Declaration { name, init } = declaration {
            if identifier_map.contains_key(name)
                && identifier_map.get(name).unwrap().from_current_scope == true
            {
                panic!("Duplicate variable declaration!");
            }

            let unique_name = self.variable_context.make_temporary(Some(name.clone()));
            identifier_map.insert(
                name.clone(),
                MapEntry {
                    unique_name: unique_name.clone(),
                    from_current_scope: true,
                    has_linkage: false,
                },
            );

            let resolved_init = if !matches!(init.as_ref(), ast::AstNode::NULL) {
                self.resolve_exp(init.as_ref(), identifier_map)
            } else {
                *init.clone()
            };

            ast::AstNode::Declaration {
                name: unique_name,
                init: Box::new(resolved_init),
            }
        } else if let ast::AstNode::FunctionDecl { .. } = declaration {
            return self.resolve_function_declaration(declaration.clone(), identifier_map);
        } else {
            panic!("Expected declaration!");
        }
    }

    fn resolve_statement(
        &mut self,
        statement: &ast::AstNode,
        current_loop_id: Option<i32>,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> ast::AstNode {
        match statement {
            ast::AstNode::Return { exp } => {
                return ast::AstNode::Return {
                    exp: Box::new(self.resolve_exp(exp, identifier_map)),
                };
            }

            ast::AstNode::Expression { exp } => {
                return ast::AstNode::Expression {
                    exp: Box::new(self.resolve_exp(exp, identifier_map)),
                };
            }

            ast::AstNode::If {
                condition,
                then,
                else_maybe,
            } => {
                return ast::AstNode::If {
                    condition: Box::new(self.resolve_exp(condition, identifier_map)),
                    then: Box::new(self.resolve_statement(then, current_loop_id, identifier_map)),
                    else_maybe: Box::new(self.resolve_statement(
                        else_maybe,
                        current_loop_id,
                        identifier_map,
                    )),
                };
            }

            ast::AstNode::Compound { block } => {
                let mut identifier_map_copy = identifier_map.clone();

                for map_entry in identifier_map_copy.values_mut() {
                    if map_entry.from_current_scope == true {
                        map_entry.from_current_scope = false;
                    }
                }
                return ast::AstNode::Compound {
                    block: self.resolve_block(
                        block.clone(),
                        current_loop_id,
                        &mut identifier_map_copy,
                    ),
                };
            }

            ast::AstNode::For {
                init,
                condition,
                step,
                body,
                id,
            } => {
                let mut identifier_map_copy = identifier_map.clone();

                for map_entry in identifier_map_copy.values_mut() {
                    if map_entry.from_current_scope == true {
                        map_entry.from_current_scope = false;
                    }
                }

                return ast::AstNode::For {
                    init: Box::new(self.resolve_block_item(
                        *init.clone(),
                        None,
                        &mut identifier_map_copy,
                    )),
                    condition: Box::new(self.resolve_exp(condition, &identifier_map_copy)),
                    step: Box::new(self.resolve_exp(step, &identifier_map_copy)),
                    body: Box::new(self.resolve_statement(
                        body,
                        Some(id.clone()),
                        &mut identifier_map_copy,
                    )),
                    id: id.clone(),
                };
            }

            ast::AstNode::While {
                condition,
                body,
                id,
            } => {
                let mut identifier_map_copy = identifier_map.clone();

                for map_entry in identifier_map_copy.values_mut() {
                    if map_entry.from_current_scope == true {
                        map_entry.from_current_scope = false;
                    }
                }

                return ast::AstNode::While {
                    condition: Box::new(self.resolve_exp(condition, &identifier_map_copy)),
                    body: Box::new(self.resolve_statement(
                        body,
                        Some(id.clone()),
                        &mut identifier_map_copy,
                    )),
                    id: id.clone(),
                };
            }

            ast::AstNode::DoWhile {
                body,
                condition,
                id,
            } => {
                let mut identifier_map_copy = identifier_map.clone();

                for map_entry in identifier_map_copy.values_mut() {
                    if map_entry.from_current_scope == true {
                        map_entry.from_current_scope = false;
                    }
                }

                return ast::AstNode::DoWhile {
                    body: Box::new(self.resolve_statement(
                        body,
                        Some(id.clone()),
                        &mut identifier_map_copy,
                    )),
                    condition: Box::new(self.resolve_exp(condition, &identifier_map_copy)),
                    id: id.clone(),
                };
            }

            ast::AstNode::Break { .. } => {
                return ast::AstNode::Break {
                    id: current_loop_id.unwrap(),
                };
            }

            ast::AstNode::Continue { .. } => {
                return ast::AstNode::Continue {
                    id: current_loop_id.unwrap(),
                };
            }

            ast::AstNode::NULL => {
                return statement.clone();
            }
            _ => {
                return statement.clone();
            }
        }
    }

    // fn label_statement(&mut self, statement: ast::AstNode, current_loop_id: i32) -> ast::AstNode {
    //     match statement {
    //         ast::AstNode::Compound { block } => {
    //             let mut new_block = Vec::<ast::AstNode>::new();

    //             for statement in block {
    //                 new_block.push(self.label_statement(statement, current_label.clone()));
    //             }

    //             return ast::AstNode::Compound { block: new_block };
    //         }

    //         ast::AstNode::Break { .. } => {
    //             return ast::AstNode::Break {
    //                 id: current_loop_id,
    //             };
    //         }

    //         ast::AstNode::Continue { .. } => {
    //             return ast::AstNode::Continue {
    //                 id: current_loop_id,
    //             };
    //         }

    //         _ => {
    //             panic!("label error!");
    //         }
    //     }
    // }

    fn resolve_exp(
        &self,
        exp: &ast::AstNode,
        identifier_map: &HashMap<String, MapEntry>,
    ) -> ast::AstNode {
        match exp {
            ast::AstNode::Assignment { exp1, exp2 } => {
                if !matches!(exp1.as_ref(), ast::AstNode::Var { .. }) {
                    panic!("Invalid lvalue: {:?}", exp1);
                }

                return ast::AstNode::Assignment {
                    exp1: Box::new(self.resolve_exp(exp1, identifier_map)),
                    exp2: Box::new(self.resolve_exp(exp2, identifier_map)),
                };
            }

            ast::AstNode::Conditional {
                condition,
                if_exp,
                else_exp,
            } => {
                return ast::AstNode::Conditional {
                    condition: Box::new(self.resolve_exp(condition, identifier_map)),
                    if_exp: Box::new(self.resolve_exp(if_exp, identifier_map)),
                    else_exp: Box::new(self.resolve_exp(else_exp, identifier_map)),
                };
            }

            ast::AstNode::Var { identifier } => {
                if identifier_map.contains_key(identifier) {
                    return ast::AstNode::Var {
                        identifier: identifier_map.get(identifier).unwrap().unique_name.clone(),
                    };
                } else {
                    panic!("Invalid lvalue: {:?}", exp);
                }
            }

            ast::AstNode::Binary {
                binary_operator,
                left,
                right,
            } => {
                return ast::AstNode::Binary {
                    binary_operator: binary_operator.clone(),
                    left: Box::new(self.resolve_exp(left, identifier_map)),
                    right: Box::new(self.resolve_exp(right, identifier_map)),
                };
            }

            ast::AstNode::Unary {
                unary_operator,
                exp,
            } => {
                return ast::AstNode::Unary {
                    unary_operator: unary_operator.clone(),
                    exp: Box::new(self.resolve_exp(exp, identifier_map)),
                };
            }

            ast::AstNode::FunctionCall { identifier, args } => {
                if identifier_map.get(identifier).is_some() {
                    //这是为了防止变量被当作函数去调用
                    //如果这是变量，new_fun_name就会是var_1此类
                    //这样在type_check阶段检查类型时就能发现问题
                    //若是函数，new_fun_name保持不变
                    let new_fun_name = identifier_map.get(identifier).unwrap().unique_name.clone();

                    let mut new_args = Vec::<ast::AstNode>::new();
                    for arg in args {
                        new_args.push(self.resolve_exp(arg, identifier_map));
                    }

                    return ast::AstNode::FunctionCall {
                        identifier: new_fun_name,
                        args: new_args,
                    };
                } else {
                    panic!("Undeclared function {:?}", exp);
                }
            }

            //TODO: 加入对(可能还有别的, 不确定)的resolve
            _ => {
                return exp.clone();
            }
        }
    }

    // =======================
    //   Type Checking Part is Below
    // =======================

    fn type_check_var_decl(&mut self, decl: &ast::AstNode) {
        match decl {
            ast::AstNode::Declaration { name, init } => {
                self.symbol_table.insert(name.clone(), Type::Int);

                if matches!(**init, ast::AstNode::NULL) {
                    self.type_check_exp(init);
                }
            }

            _ => {
                panic!("");
            }
        }
    }

    fn type_check_func_decl(&mut self, decl: &ast::AstNode) {
        match decl {
            ast::AstNode::FunctionDecl { name, params, body } => {
                let has_body = !matches!(**body, ast::AstNode::NULL);

                let mut already_defined = false;

                if self.symbol_table.contains_key(name) {
                    let old_decl = self.symbol_table.get(name);

                    //若符号已经声明过且不是函数类型（那就是变量）
                    if let Some(Type::FunType {
                        param_count,
                        defined,
                    }) = old_decl
                    {
                        already_defined = *defined;
                    } else {
                        panic!("Incompatible function declarations");
                    }
                }

                //函数重复定义
                if already_defined && has_body {
                    panic!("Function is defined more than once");
                }

                self.symbol_table.insert(
                    name.clone(),
                    Type::FunType {
                        param_count: params.len() as i32,
                        defined: already_defined || has_body,
                    },
                );

                if has_body {
                    for param in params {
                        self.symbol_table.insert(param.clone(), Type::Int);
                    }

                    self.type_check_compound(body);
                }
            }

            _ => {
                panic!("");
            }
        }
    }

    fn type_check_exp(&mut self, exp: &ast::AstNode) {
        match exp {
            ast::AstNode::FunctionCall { identifier, args } => {
                if let Some(func_type) = self.symbol_table.get(identifier) {
                    match func_type {
                        Type::Int => {
                            panic!("变量被用作函数名: {}", identifier);
                        }
                        Type::FunType { param_count, .. } => {
                            if *param_count != args.len() as i32 {
                                panic!(
                                    "函数调用参数数量错误: {} 期望 {}, 实际 {}",
                                    identifier,
                                    param_count,
                                    args.len()
                                );
                            }
                            for arg in args {
                                self.type_check_exp(arg);
                            }
                        }
                    }
                } else {
                    panic!("未定义函数: {}", identifier);
                }
            }

            ast::AstNode::Var { identifier } => {
                if self.symbol_table.get(identifier).is_none() {
                    panic!("未定义变量: {}", identifier);
                }

                if !matches!(self.symbol_table.get(identifier).unwrap(), Type::Int) {
                    panic!("函数名被用作变量: {}", identifier);
                }
            }

            ast::AstNode::Binary {
                binary_operator,
                left,
                right,
            } => {
                self.type_check_exp(&left);
                self.type_check_exp(&right);
            }

            ast::AstNode::Expression { exp } | ast::AstNode::Return { exp } => {
                self.type_check_exp(exp);
            }

            _ => {
                // panic!("未实现 type_check_exp: {:?}", exp);
            }
        }
    }

    fn type_check_compound(&mut self, compound: &ast::AstNode) {
        match compound {
            ast::AstNode::Compound { block } => {
                for block_item in block {
                    match block_item {
                        ast::AstNode::Declaration { .. } => {
                            self.type_check_var_decl(block_item);
                        }
                        ast::AstNode::FunctionDecl { .. } => {
                            self.type_check_func_decl(block_item);
                        }
                        _ => {
                            self.type_check_exp(block_item);
                        }
                    }
                }
            }

            _ => {
                panic!("");
            }
        }
    }
}
