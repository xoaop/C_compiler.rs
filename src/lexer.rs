use regex::Regex;
use std::{fs, usize};


#[derive(Debug)]
#[derive(Clone)]
pub enum TokenType {
    Integer(i32), //Integer 1234
    Identifier(String), //Identifier main
    LelfBracket, //Left Bracket (
    RightBracket, //Right Bracket )
    LcurlyBracket, //Left Curly Bracket {
    RcurlyBracket, //Right Curly Bracket }
    Semicolon, //Semicolon ;
    KeywordInt,
    KeywordVoid,
    KeywordReturn,
    Tilde, // ~
    Hyphen, // -
    TwoHyphen,// --
    Plus,      // +
    Asterisk,      // *
    ForwardSlash,     // /
    Percent,   // %
    // Other,
}

impl TokenType {
    pub fn is_binary_operator(&self) -> bool {
        match self {
            TokenType::Plus
            | TokenType::Hyphen
            | TokenType::Asterisk
            | TokenType::ForwardSlash
            | TokenType::Percent => true,
            _ => false,
        }
            
    }
}


// impl TokenType {
//     pub fn as_value<T: 'static>(&self) -> Option<&T> {
//             match self {
//                 TokenType::Integer(value) if std::any::TypeId::of::<T>() == std::any::TypeId::of::<i64>() => {
//                     Some(unsafe { &*(value as *const i32 as *const T) })
//                 }
//                 TokenType::Identifier(name) if std::any::TypeId::of::<T>() == std::any::TypeId::of::<String>() => {
//                     Some(unsafe { &*(name as *const String as *const T) })
//                 }
//                 _ => None,
//             }
//         }
// }





#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
}

pub struct TokenList {
    index: usize,
    tokens: Vec<Token>,
}

impl TokenList{
    pub fn new() -> Self {
        TokenList { index: 0 ,tokens: Vec::new() }
    }

    // 添加一个 token 到 TokenList 的成员函数
    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn iter(&self) -> std::slice::Iter<Token> {
        return self.tokens.iter();
    }

    pub fn next_token(&mut self) -> Option<&Token> {
        if self.index < self.tokens.len() {
            let token = self.tokens.get(self.index);
            self.index += 1;
            return token;
        }
        return None;
    }

    pub fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    pub fn forward(&mut self) {
        self.index += 1;
    }

    pub fn back(&mut self) {
        self.index -= 1;
    }

    pub fn reset(&mut self) {
        self.index = 0;
    }

}





fn get_regex_map() -> Vec<(&'static str, TokenType)> {
    // 注意先后顺序
    vec![
        (r"^--", TokenType::TwoHyphen),
        (r"^\+", TokenType::Plus),
        (r"^\*", TokenType::Asterisk),
        (r"^/", TokenType::ForwardSlash),
        (r"^%", TokenType::Percent),
        (r"^-", TokenType::Hyphen),
        (r"^~", TokenType::Tilde),
        (r"^\(", TokenType::LelfBracket),
        (r"^\)", TokenType::RightBracket),
        (r"^\{", TokenType::LcurlyBracket),
        (r"^}", TokenType::RcurlyBracket),
        (r"^;", TokenType::Semicolon),
        (r"^int\b", TokenType::KeywordInt),
        (r"^void\b", TokenType::KeywordVoid),
        (r"^return\b", TokenType::KeywordReturn),
        (r"^[0-9]+\b", TokenType::Integer(0)),
        (r"^[a-zA-Z_]\w*\b", TokenType::Identifier("".to_string())),
    ]
}

pub fn get_token_list(path: &str) -> Option<TokenList> {
    let mut tokens = TokenList::new();

    let contents = fs::read_to_string(path).expect("Unable to read file");

    let mut contents: &str = &contents;

    let regex_map = get_regex_map();

    loop {
        // 去掉开头的空格
        contents = contents.trim_start();

        // 判断条件，如果字符串为空则退出循环
        if contents.is_empty() {
            break;
        }

        let mut success = false;
        // 接下来进行其他操作，例如匹配正则表达式等
        for (regex_str, token_type) in &regex_map {
            let regex = Regex::new(regex_str).expect("Invalid regex");

            if let Some(_mat) = regex.find(contents) {
                let new_token_type = match token_type {
                    TokenType::Identifier(_) => TokenType::Identifier(_mat.as_str().to_string()),
                    TokenType::Integer(_) => {
                        match _mat.as_str().parse::<i32>() {
                            Ok(parsed_val) => TokenType::Integer(parsed_val),
                            Err(_) => {
                                eprintln!("Failed to parse integer token");
                                std::process::exit(1);
                            }
                        }
                    },
                    _ => token_type.clone()
                };
                
                tokens.push(Token {
                    token_type: new_token_type
                });

                contents = &contents[_mat.end()..];
                success = true;
                break;
            }
        }
        if !success {
            eprintln!("遇到未知的token");
            std::process::exit(1);
        }

    }
        
    return Some(tokens);
}
