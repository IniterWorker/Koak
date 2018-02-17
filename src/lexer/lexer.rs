//!
//! Koak's lexer
//!

use std::rc::Rc;
use std::iter::Peekable;
use std::str::Chars;

use lexer::{Token, TokenType, OperatorType, LexerResult};
use error::{SyntaxError, ErrorReason};
use lang::Type;

///
/// Iterator over chars used by the lexer
///
pub struct LexerCharIterator<'a> {
    chars: Peekable<Chars<'a>>,
    col: usize,
}

impl<'a> LexerCharIterator<'a> {
    #[inline]
    pub fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|x| *x)
    }

    #[inline]
    pub fn get_col(&self) -> usize {
        self.col
    }
}

impl<'a> Iterator for LexerCharIterator<'a> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.col += 1;
        self.chars.next()
    }
}

///
/// Lexing engine
///
pub struct Lexer<'a> {
    chars: LexerCharIterator<'a>,
    line: Rc<String>,
    row: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(line: &'a Rc<String>, row: usize) -> Lexer {
        Lexer {
            chars: LexerCharIterator {
                chars: line.chars().peekable(),
                col: 0,
            },
            line: line.clone(),
            col: 0,
            row: row,
        }
    }

    #[inline]
    fn new_token(&self, tt: TokenType) -> Token {
        Token::from(
            tt,
            self.line.clone(),
            self.row,
            (self.col, self.chars.get_col())
        )
    }

    #[inline]
    fn new_syntaxerror(&self, er: ErrorReason) -> SyntaxError {
        SyntaxError::new(
            er,
            self.line.clone(),
            self.row,
            (self.col, self.chars.get_col())
        )
    }

    fn lex_comment(&mut self) {
        while let Some(c) = self.chars.next() {
            if c == '\n' {
                break;
            }
        }
    }

    fn lex_number(&mut self, c: char) -> LexerResult {
        let mut s = c.to_string();
        let mut base = 10;
        let mut base_len = 0;
        let mut is_double = false;

        // Try to determine the base used in the litteral number
        if c == '0' {
            match self.chars.peek() {
                Some(c @ 'x') | Some(c @ 'X') => {
                    base = 16;
                    self.chars.next();
                    s.push(c);
                    base_len = 2;
                },
                _ => {
                    base = 8;
                },
            }
        }

        // Append digits
        while let Some(c) = self.chars.peek() {
            match c {
                '0'...'9' | 'a'...'z' | 'A'...'Z' | '.' => {
                    is_double |= c == '.';
                    self.chars.next();
                    s.push(c);
                }
                _ => break,
            }
        }

        // Convert string to number
        if is_double {
            match s.parse::<f64>().ok() {
                Some(f) => Ok(self.new_token(TokenType::DoubleLitteral(f))),
                _ => Err(self.new_syntaxerror(ErrorReason::InvalidLitteralNum(s))),
            }
        } else {
            match i32::from_str_radix(&s[base_len..], base).ok() {
                Some(i) => Ok(self.new_token(TokenType::IntegerLitteral(i))),
                _ => Err(self.new_syntaxerror(ErrorReason::InvalidLitteralNum(s))),
            }
        }
    }

    fn lex_identifier(&mut self, c: char) -> LexerResult {
        let mut s = c.to_string();

        while let Some(c) = self.chars.peek() {
            match c {
                '0'...'9' | 'a'...'z' | 'A'...'Z' | '_' => {
                    self.chars.next();
                    s.push(c);
                }
                _ => break,
            }
        }
        match s.as_ref() {
            "def" => Ok(self.new_token(TokenType::Def)),
            "extern" => Ok(self.new_token(TokenType::Extern)),
            "if" => Ok(self.new_token(TokenType::If)),
            "then" => Ok(self.new_token(TokenType::Then)),
            "else" => Ok(self.new_token(TokenType::Else)),
            "double" => Ok(self.new_token(TokenType::Type(Type::Double))),
            "int" => Ok(self.new_token(TokenType::Type(Type::Int))),
            _ => Ok(self.new_token(TokenType::Identifier(Rc::new(s)))),
        }
    }

    fn lex_operators(&mut self, c: char) -> LexerResult {
        match (c, self.chars.peek()) {
            ('-', Some('>')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Arrow))
            }
            ('+', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Add))),
            ('-', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Sub))),
            ('*', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Mul))),
            ('/', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Div))),
            ('%', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Rem))),
            ('<', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Less))),
            ('>', _) => Ok(self.new_token(TokenType::Operator(OperatorType::More))),
            _ => unreachable!(),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexerResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next() {
            Some(c) => {
                self.col = self.chars.get_col();
                match c {
                    ' ' | '\r' | '\t' | '\n' => self.next(), // Skip whitespace
                    'a'...'z' | 'A'...'Z' | '_' => Some(self.lex_identifier(c)),
                    '0'...'9' => Some(self.lex_number(c)),
                    '+' | '-' | '*' | '/' | '%' | '>' | '<' => Some(self.lex_operators(c)),
                    '(' => Some(Ok(self.new_token(TokenType::OpenParenthesis))),
                    ')' => Some(Ok(self.new_token(TokenType::CloseParenthesis))),
                    ',' => Some(Ok(self.new_token(TokenType::Comma))),
                    ';' => Some(Ok(self.new_token(TokenType::SemiColon))),
                    ':' => Some(Ok(self.new_token(TokenType::Colon))),
                    '#' => { self.lex_comment(); self.next() },
                    _ => Some(Err(self.new_syntaxerror(ErrorReason::UnknownChar(c))))
                }
            },
            _ => None,
        }
    }
}
