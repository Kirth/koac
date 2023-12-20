#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    LParen,
    RParen,
    If,
    Else,
    Return,
    Continue,
    Switch,
    Assign,
    DQuote,
    SQuote,
    Equals,
    Plus,
    Minus,
    Times,
    Slash, // forward
    Backslash,
    Colon,
    Bang,
    Underscore,
    Nil,
    And,
    Or,
    Not,
    While,
    For,
    Fn,
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),
    Identifier(String),
    Comment(String),
    Invalid(String), // e.g. unterminated strings
}

#[derive(Clone, Copy, Debug)]
pub struct CodeLoc {
    pub line: usize,
    pub col: usize,
}
#[derive(Clone, Debug)]
pub struct TokenLoc {
    pub token: Token,
    pub start: CodeLoc,
    pub end: CodeLoc,
}

pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    start: CodeLoc,
    pos: CodeLoc,
    tokens: Vec<TokenLoc>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Lexer {
            chars: code.chars().peekable(),
            start: CodeLoc { line: 1, col: 1 },
            pos: CodeLoc { line: 1, col: 1 },
            tokens: Vec::new()
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn next(&mut self) -> Option<char> {
        let c = self.chars.next();
        match c {
            Some('\n') => {
                self.pos.line += 1;
                self.pos.col = 1;
            }
            Some(_) => {
                self.pos.col += 1;
               // self.pos.line += 1;
            }
            None => {}
        }

        return c
    }

    fn emit(&mut self, token: Token) {
        self.tokens.push(TokenLoc { token: token, start: self.start, end: self.pos });
        self.start = self.pos;  // move start of next token once emitted
    }

    fn lex_string(&mut self, delim: char) -> String {
        let mut acc = String::new();
        while self.peek() != Some(&delim) {
            match self.next() {
                Some('\\') => match self.next() {
                    Some('n') => acc.push('\n'),
                    Some('r') => acc.push('\r'),
                    Some('t') => acc.push('\t'),
                    Some('0') => acc.push('\0'),
                    Some(c @ ('\\' | '\'' | '\"')) => acc.push(c),
                    Some(c) => { println!("wtf in lex_string: \\ followed by: {}", c) },
                    None => {},
                    // todo: hex escapes
                },
                Some(c) => acc.push(c),
                None => {
                    self.emit(Token::Invalid("lexer: string lit hit EOF".to_string()));
                    break;
                }

            }
        }

        self.next(); // consume closing delimiter

        return acc
    }

    pub fn lex(&mut self) {
        while let Some(c) = self.next() {
            match c {
                '(' => self.emit(Token::LParen),
                ')' => self.emit(Token::RParen),
                '{' => self.emit(Token::LBrace),
                '}' => self.emit(Token::RBrace),
                '\\' => self.emit(Token::Backslash),
                '!' => self.emit(Token::Bang),
                '_' => self.emit(Token::Underscore),
                '+' => self.emit(Token::Plus),
                '-' => self.emit(Token::Minus),
                '*' => self.emit(Token::Times),
                ':' => self.emit(Token::Colon),
                ',' => self.emit(Token::Comma),
                ';' => self.emit(Token::Semicolon),
                '\'' | '\"' => {
                    let s = self.lex_string(c);
                    self.emit(Token::StringLit(s))
                },
                '#' => {
                    let mut comment = String::new();
                    loop {
                        match self.next() {
                            None | Some('\n') => break,
                            Some(c) => comment.push(c),
                        }
                    }
                    self.emit(Token::Comment(comment))
                }
                c => { // either a keyword or invalid
                    let mut acc = c.to_string();

                    if c.is_whitespace() {
                        continue;
                    }
                    else if c.is_digit(10) {
                        while let Some(cc) = self.peek().filter(|ccc| ccc.is_digit(10)) {
                            acc.push(*cc);
                            self.next();
                        }

                        if self.peek() == Some(&'.') { // float!
                            acc.push('.');
                            self.next(); 

                            while let Some(cc) = self.peek().filter(|ccc| ccc.is_digit(10)) {
                                acc.push(*cc);
                                self.next();
                            }

                            self.emit(Token::FloatLit(acc.parse::<f64>().unwrap()))
                        } else {
                            self.emit(Token::IntLit(acc.parse::<i64>().unwrap()))
                        }
                    }
                    else if c.is_alphabetic() {
                        while let Some(cc) = self.peek().filter(|c| 
                            c.is_alphabetic() || **c == '_' || **c == '\'' || **c == '?' || **c == '!') {
                            
                            acc.push(*cc);
                            self.next();
                        }

                        self.emit(match acc.as_str() {
                            "if" => Token::If,
                            "else" => Token::Else,
                            "while" => Token::While,
                            "for" => Token::For,
                            "and" => Token::And,
                            "or" => Token::Or,
                            "not" => Token::Not,
                            "continue" => Token::Continue,
                            "fn" => Token::Fn,
                            //"return" => Token::Return, // TODO: is this an identifier??
                            _ => Token::Identifier(acc)
                        })
                    }
                    else if "!$%&*+_./<=>?@^".contains(c) {
                        let mut acc = String::new();
                        let mut last = c;

                        while let Some(cc) = self.peek().filter(|c| "!$%&*+_./<=>?@^".contains(**c)) {
                            acc.push(last);
                            last = *cc;
                            self.next();
                        }

                        match (acc.as_str(), last) {
                            ("!" | "<" | ">" | "=", '=') => {
                                acc.push(last);
                                self.emit(Token::Identifier(acc))
                            },
                            ("", '=') => {
                                self.emit(Token::Assign)
                            }
                            (_, '=') => {
                                self.emit(Token::Identifier(acc));
                                self.emit(Token::Assign)
                            }
                            (_, _) => {
                                acc.push(last);
                                self.emit(match acc.as_str() {
                                    "!" => Token::Bang,
                                    _ => Token::Identifier(acc)
                                })
                            }
                        }
                    }
                    else {
                        self.emit(Token::Invalid(format!("lexer encountered unknown character: {}", c)))
                    }
                }
            }
        }
    }
}


pub fn lex(input: String) -> Vec<TokenLoc> {
    let mut l = Lexer::new(&input);
    l.lex();

    return l.tokens
}
