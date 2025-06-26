#[derive(Debug, Clone)]
pub enum TokenType {
    Integer(i32),       //Integer 1234
    Identifier(String), //Identifier main
    LelfBracket,        //Left Bracket (
    RightBracket,       //Right Bracket )
    LcurlyBracket,      //Left Curly Bracket {
    RcurlyBracket,      //Right Curly Bracket }
    Semicolon,          //Semicolon ;
    Comma, //逗号 ,

    KeywordInt,
    KeywordVoid,
    KeywordReturn,
    KeywordIf,       // if
    KeywordElse,     // else
    KeywordDo,       // do
    KeywordWhile,    // while
    KeywordFor,      // for
    KeywordBreak,    // break
    KeywordContinue, // continue

    Tilde,       // ~
    Hyphen,      // -
    TwoPlus,    // ++
    TwoHyphen,   // --
    Exclamation, // !

    Plus,         // +
    Asterisk,     // *
    ForwardSlash, // /
    Percent,      // %
    TwoAnd,       // &&
    TwoOr,        // ||
    TwoEqual,     // ==
    NotEqual,     // !=
    Less,         // <
    Greater,      // >
    LessEqual,    // <=
    GreaterEqual, // >=

    Equal,

    QuestionMark, // ?
    Colon,        // :

    DoubleForwardSlash, // itself(
}

impl TokenType {
    pub fn is_binary_operator(&self) -> bool {
        match self {
            TokenType::Plus
            | TokenType::Hyphen
            | TokenType::Asterisk
            | TokenType::ForwardSlash
            | TokenType::Percent
            | TokenType::TwoAnd
            | TokenType::TwoOr
            | TokenType::TwoEqual
            | TokenType::NotEqual
            | TokenType::Less
            | TokenType::Greater
            | TokenType::LessEqual
            | TokenType::GreaterEqual
            | TokenType::Equal
            | TokenType::QuestionMark => true,
            _ => false,
        }
    }

}

#[derive(Debug)]
pub struct Token {
    pub index: i32,
    pub token_type: TokenType,
}

#[derive(Debug)]
pub struct TokenList {
    index: usize,
    tokens: Vec<Token>,
}

impl TokenList {
    pub fn new() -> Self {
        TokenList {
            index: 0,
            tokens: Vec::new(),
        }
    }

    // 添加一个 token 到 TokenList 的成员函数
    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }

    // pub fn iter(&self) -> std::slice::Iter<Token> {
    //     return self.tokens.iter();
    // }

    pub fn next_token(&mut self) -> Option<&Token> {
        if self.index < self.tokens.len() {
            let token = self.tokens.get(self.index);
            self.index += 1;
            return token;
        }

        panic!("Tokenlist is at the end!");
        // return None;
    }

    pub fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    // pub fn forward(&mut self) {
    //     self.index += 1;
    // }

    pub fn back(&mut self) {
        self.index -= 1;
    }

    // pub fn reset(&mut self) {
    //     self.index = 0;
    // }
}
