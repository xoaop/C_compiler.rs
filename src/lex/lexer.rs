use regex::Regex;
use std::fs;

// Import or define TokenType and TokenList here
use crate::lex::token::{Token, TokenList, TokenType};

fn get_regex_map() -> Vec<(&'static str, TokenType)> {
    // 注意先后顺序：长的在前，短的在后；关键字在前，标识符在后
    vec![
        (r"^//", TokenType::DoubleForwardSlash),
        (r"^&&", TokenType::TwoAnd),
        (r"^\|\|", TokenType::TwoOr),
        (r"^==", TokenType::TwoEqual),
        (r"^!=", TokenType::NotEqual),
        (r"^<=", TokenType::LessEqual),
        (r"^>=", TokenType::GreaterEqual),
        (r"^\+\+", TokenType::TwoPlus),
        (r"^--", TokenType::TwoHyphen),
        (r"^=", TokenType::Equal),
        (r"^<", TokenType::Less),
        (r"^>", TokenType::Greater),
        (r"^\+", TokenType::Plus),
        (r"^\*", TokenType::Asterisk),
        (r"^/", TokenType::ForwardSlash),
        (r"^%", TokenType::Percent),
        (r"^-", TokenType::Hyphen),
        (r"^~", TokenType::Tilde),
        (r"^!", TokenType::Exclamation),
        (r"^\(", TokenType::LelfBracket),
        (r"^\)", TokenType::RightBracket),
        (r"^\{", TokenType::LcurlyBracket),
        (r"^}", TokenType::RcurlyBracket),
        (r"^;", TokenType::Semicolon),
        (r"^\?", TokenType::QuestionMark),
        (r"^:", TokenType::Colon),
        (r"^if\b", TokenType::KeywordIf),
        (r"^else\b", TokenType::KeywordElse),
        (r"^int\b", TokenType::KeywordInt),
        (r"^void\b", TokenType::KeywordVoid),
        (r"^return\b", TokenType::KeywordReturn),
        (r"^do\b", TokenType::KeywordDo),
        (r"^while\b", TokenType::KeywordWhile),
        (r"^for\b", TokenType::KeywordFor),
        (r"^break\b", TokenType::KeywordBreak),
        (r"^continue\b", TokenType::KeywordContinue),
        (r"^[0-9]+\b", TokenType::Integer(0)),
        (r"^[a-zA-Z_]\w*\b", TokenType::Identifier("".to_string())),
    ]
}

pub fn get_token_list(path: &str) -> Option<TokenList> {
    let mut tokens = TokenList::new();

    let contents = fs::read_to_string(path).expect("Unable to read file");

    let mut contents: &str = &contents;

    let regex_map = get_regex_map();

    let mut token_counter = 0;

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
                    TokenType::Integer(_) => match _mat.as_str().parse::<i32>() {
                        Ok(parsed_val) => TokenType::Integer(parsed_val),
                        Err(_) => {
                            eprintln!("Failed to parse integer token");
                            std::process::exit(1);
                        }
                    },
                    _ => token_type.clone(),
                };

                tokens.push(Token {
                    index: token_counter,
                    token_type: new_token_type,
                });
                token_counter += 1;

                contents = &contents[_mat.end()..];
                success = true;
                break;
            }
        }
        if !success {
            panic!("遇到未知的token: {}", contents);
        }
    }

    return Some(tokens);
}
