use std::fmt;

#[path = "error.rs"]
mod error;

use error::SyntaxError;

#[derive(Debug, Clone)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    Str,
    Number,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    CommentLine,
    Eof,
}

pub enum Literal {
    Number(f64),
    Str(String),
    Keyword,
    Symbol(String, TokenType),
    Identifier(String),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Number(_n) => write!(f, "LiteralNumber"),
            Literal::Str(_s) => write!(f, "LiteralString"),
            Literal::Symbol(_s, _tt) => write!(f, "LiteralSymbol"),
            Literal::Keyword => write!(f, "Keyword"),
            Literal::Identifier(_s) => write!(f, "Identifier"),
        }
    }
}

pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub literal: Literal,
    pub line: u32,
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: String, literal: Literal, line: u32) -> Self {
        Token {
            ttype,
            lexeme,
            literal,
            line,
        }
    }

    fn literal_from_s(ctx: &Context, lexeme: String) -> Self {
        let (ttype, literal) = match lexeme.as_str() {
            "and" => (TokenType::And, Literal::Keyword),
            "class" => (TokenType::Class, Literal::Keyword),
            "else" => (TokenType::Else, Literal::Keyword),
            "false" => (TokenType::False, Literal::Keyword),
            "for" => (TokenType::For, Literal::Keyword),
            "if" => (TokenType::If, Literal::Keyword),
            "nil" => (TokenType::Nil, Literal::Keyword),
            "or" => (TokenType::Or, Literal::Keyword),
            "print" => (TokenType::Print, Literal::Keyword),
            "return" => (TokenType::Return, Literal::Keyword),
            "super" => (TokenType::Super, Literal::Keyword),
            "this" => (TokenType::This, Literal::Keyword),
            "true" => (TokenType::True, Literal::Keyword),
            "var" => (TokenType::Var, Literal::Keyword),
            "while" => (TokenType::While, Literal::Keyword),
            _ => (TokenType::Identifier, Literal::Identifier(lexeme.clone())),
        };

        Token {
            ttype,
            lexeme,
            literal,
            line: ctx.line,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {} {}", self.ttype, self.lexeme, self.literal)
    }
}

// Contains the scan context, i.e. location in the character stream
// that we're scanning.
struct Context<'a> {
    current: usize,
    start: usize,
    line: u32,
    source: Box<&'a str>,
}

fn build_token(ctx: &mut Context, ttype: TokenType) -> Token {
    ctx.current += 1;
    let code = ctx.source[ctx.start..ctx.current].to_string();
    Token::new(
        ttype.clone(),
        code.clone(),
        Literal::Symbol(code, ttype),
        ctx.line,
    )
}

/// conditionally advances the current cursor position if query == next
fn match_next(ctx: &mut Context, query: char) -> bool {
    match ctx.source.chars().nth(ctx.current + 1) {
        Some(c) => {
            if c != query {
                return false;
            }
            ctx.current += 1;
            true
        }
        None => false,
    }
}

/// maybe build the next token if it matches a pattern
fn build_if_match(
    ctx: &mut Context,
    query: char,
    match_ttype: TokenType,
    ttype: TokenType,
) -> Token {
    match match_next(ctx, query) {
        true => build_token(ctx, match_ttype),
        false => build_token(ctx, ttype),
    }
}

enum AdvanceResult {
    Terminated(String),
    Unterminated(String),
}

fn advance_while(ctx: &mut Context, cond: fn(char) -> bool) -> AdvanceResult {
    let mut buf = String::new();
    loop {
        match ctx.source.chars().nth(ctx.current) {
            Some(c) => {
                if c == '\n' {
                    ctx.line += 1;
                }
                if !cond(c) {
                    return AdvanceResult::Terminated(buf);
                }
                buf.push(c);
            },
            None => break,
        };
        ctx.current += 1;
    }

    match cond('\0') {
        true => AdvanceResult::Unterminated(buf),
        false => AdvanceResult::Terminated(buf),
    }
}

fn advance_until(ctx: &mut Context, until: char) -> AdvanceResult {
    let mut buf = String::new();
    loop {
        match ctx.source.chars().nth(ctx.current) {
            Some(c) => {
                if c == '\n' {
                    ctx.line += 1;
                }
                if c == until {
                    ctx.current += 1;
                    return AdvanceResult::Terminated(buf);
                }
                buf.push(c);
            },
            None => break,
        };
        ctx.current += 1;
    }

    AdvanceResult::Unterminated(buf)
}

enum ScanResult {
    Discard,
    Token(Token),
    Error(SyntaxError),
}

fn scan_token(ctx: &mut Context) -> ScanResult {
    let c = ctx.source.chars().nth(ctx.start);
    if let None = c {
        return ScanResult::Discard;
    }
    let c = c.unwrap();

    match c {
        '(' => ScanResult::Token(build_token(ctx, TokenType::LeftParen)),
        ')' => ScanResult::Token(build_token(ctx, TokenType::RightParen)),
        '{' => ScanResult::Token(build_token(ctx, TokenType::LeftBrace)),
        '}' => ScanResult::Token(build_token(ctx, TokenType::RightBrace)),
        ',' => ScanResult::Token(build_token(ctx, TokenType::Comma)),
        '.' => ScanResult::Token(build_token(ctx, TokenType::Dot)),
        '-' => ScanResult::Token(build_token(ctx, TokenType::Minus)),
        '+' => ScanResult::Token(build_token(ctx, TokenType::Plus)),
        ';' => ScanResult::Token(build_token(ctx, TokenType::Semicolon)),
        '*' => ScanResult::Token(build_token(ctx, TokenType::Star)),
        '!' => ScanResult::Token(build_if_match(
            ctx,
            '=',
            TokenType::BangEqual,
            TokenType::Bang,
        )),
        '=' => ScanResult::Token(build_if_match(
            ctx,
            '=',
            TokenType::EqualEqual,
            TokenType::Equal,
        )),
        '<' => ScanResult::Token(build_if_match(
            ctx,
            '=',
            TokenType::LessEqual,
            TokenType::Less,
        )),
        '>' => ScanResult::Token(build_if_match(
            ctx,
            '=',
            TokenType::GreaterEqual,
            TokenType::Greater,
        )),
        '/' => {
            if match_next(ctx, '/') {
                ctx.current += 1;
                advance_until(ctx, '\n'); 
                return ScanResult::Discard;
            }
            ScanResult::Token(build_token(ctx, TokenType::Slash))
        },
        ' ' => {
            ctx.current += 1;
            ScanResult::Discard
        },
        '\r' => {
            ctx.current += 1;
            ScanResult::Discard
        },
        '\t' => {
            ctx.current += 1;
            ScanResult::Discard
        },
        '\n' => {
            ctx.current += 1;
            ctx.line += 1;
            ScanResult::Discard
        },
        '"' => {
            ctx.current += 1;
            match advance_until(ctx, '"') {
                AdvanceResult::Terminated(s) => ScanResult::Token(
                    Token::new(TokenType::Str, s.clone(), Literal::Str(s), ctx.line)
                ),
                AdvanceResult::Unterminated(s) => ScanResult::Error(SyntaxError {
                    line: ctx.line,
                    code: s,
                    message: "Unterminated string.".to_string(),
                })
            }
        },
        c if char::is_ascii_digit(&c) => {
            match advance_while(ctx, |cc| {
                !"\0\t\r\n ".contains(cc) &&
                char::is_ascii_digit(&cc)
            }) {
                AdvanceResult::Terminated(s) => {
                    let result = s.parse::<f64>();
                    if let Err(_e) = result {
                        return ScanResult::Error(SyntaxError {
                            line: ctx.line,
                            code: s.clone(),
                            message: format!("Invalid number: {}", s)
                        });
                    }
                    let n = result.unwrap();
                    ScanResult::Token(Token::new(TokenType::Number, s, Literal::Number(n), ctx.line))
                },
                AdvanceResult::Unterminated(s) => ScanResult::Error(SyntaxError {
                    line: ctx.line,
                    code: s,
                    message: "Unterminated number.".to_string(),
                })
            }
        },
        c if char::is_ascii_alphabetic(&c) => {
            match advance_while(ctx, |cc| {
                !"\0\t\r\n ".contains(cc) &&
                char::is_alphanumeric(cc)
            }) {
                AdvanceResult::Terminated(s) => {
                    ScanResult::Token(Token::literal_from_s(ctx, s))
                },
                AdvanceResult::Unterminated(s) => {
                    panic!("Unterminated identifiers shouldn't be possible.");
                }
            }
        },
        _ => {
            ctx.current += 1;
            ScanResult::Error(SyntaxError {
                line: ctx.line,
                code: "".to_string(),
                message: format!("Unrecognized token: {}", c),
            }
            .into())
        }
    }
}

pub fn scan(source: String) -> (Box<Vec<Token>>, Box<Vec<SyntaxError>>) {
    let mut ctx = Context {
        start: 0,
        current: 0,
        line: 1,
        source: Box::new(source.as_str()),
    };
    let mut tokens: Vec<Token> = Vec::new();
    let mut errors: Vec<SyntaxError> = Vec::new();
    while ctx.current <= ctx.source.len()-1 {
        ctx.start = ctx.current;
        match scan_token(&mut ctx) {
            ScanResult::Token(t) => tokens.push(t),
            ScanResult::Error(e) => errors.push(e),
            ScanResult::Discard => {},
        };
    }

    (Box::new(tokens), Box::new(errors))
}
