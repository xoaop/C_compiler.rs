use regex::Regex;
use std::fs;

use crate::lex::token::{Token, TokenList, TokenType};

fn get_regex_map() -> Vec<(&'static str, TokenType)> {
    // 注意先后顺序：长的在前，短的在后；关键字在前，标识符在后
    vec![
        // parse阶段用不到的
        (r"^//", TokenType::DoubleForwardSlash),
        (r"^\r?\n", TokenType::Newline),
        (r"^/\*", TokenType::ForwardSlashAndStar),

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
        (r"^,", TokenType::Comma),
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
        (r"^static\b", TokenType::KeywordStatic),
        (r"^extern\b", TokenType::KeywordExtern),
        (r"^[0-9]+\b", TokenType::Integer(0)),
        (r"^[a-zA-Z_]\w*\b", TokenType::Identifier("".to_string())),


    ]
}

pub fn get_token_list(path: &str) -> Option<TokenList> {
    let mut tokens = TokenList::new();

    let contents = fs::read_to_string(path).expect("Unable to read file");

    let mut contents: &str = &contents;

    let regex_map = get_regex_map();

    let mut column_counter = 0;
    let mut line_counter = 0;

    loop {
        // 去掉开头的空格
        contents = contents.trim_start_matches(|c| c == ' ' || c == '\t');

        // 判断条件，如果字符串为空则退出循环
        if contents.is_empty() {
            break;
        }

        // 接下来进行其他操作，例如匹配正则表达式等
        let mut success = false;
        for (regex_str, token_type) in &regex_map {
            let regex = Regex::new(regex_str).expect("Invalid regex");

            let matched_str = regex.find(contents);
            if matched_str.is_none() {
                continue;
            }
            let matched_str = matched_str.unwrap();
            
            let new_token_type = match token_type {
                TokenType::Identifier(_) => TokenType::Identifier(matched_str.as_str().to_string()),
                TokenType::Integer(_) => match matched_str.as_str().parse::<i32>() {
                    Ok(parsed_val) => TokenType::Integer(parsed_val),
                    Err(_) => panic!("Failed to parse integer token")
                },

                TokenType::Newline => {
                    line_counter += 1;
                    column_counter = 0;
                    contents = &contents[matched_str.end()..];
                    success = true;
                    break;
                }

                TokenType::DoubleForwardSlash => {
                    // 跳过注释整行
                    if let Some(pos) = contents.find('\n') {
                        column_counter = 0;
                        contents = &contents[pos..];
                        success = true;
                    } else {
                        contents = "";
                    }
                    break;
                }

                TokenType::ForwardSlashAndStar => {
                    success = true;
                    if let Some(end_pos) = contents.find("*/") {
                        // 跳过注释块，包括"/*"和"*/"
                        let skip_len = end_pos + 2;
                        // 统计注释块内的换行符数量
                        let comment_block = &contents[..skip_len];
                        let newlines = comment_block.matches('\n').count();
                        line_counter += newlines as i32;
                        if newlines > 0 {
                            // 如果有换行，重置列号
                            column_counter = 0;
                        }
                        contents = &contents[skip_len..];
                    } else {
                        // 没有找到结尾，跳过剩余内容
                        contents = "";
                    }
                    break;
                }

                _ => token_type.clone(),
            };

            tokens.push(Token {
                line_index: line_counter,
                column_index: column_counter,
                token_type: new_token_type,
            });
            column_counter += 1;

            contents = &contents[matched_str.end()..];
            success = true;
            break;
        }
        
        if !success {
            panic!("遇到未知的token: {}", contents);
        }
    }

    return Some(tokens);
}
