//!
//! Koak's lexer
//!

use std::rc::Rc;
use std::iter::Peekable;
use std::str::Chars;

use lexer::{Token, TokenType, OperatorType, LexerResult};
use error::{SyntaxError, ErrorReason};

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
        self.chars.peek().cloned()
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

        // Try to determine the base used in the Literal number
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
                Some(f) => Ok(self.new_token(TokenType::DoubleLiteral(f))),
                _ => Err(self.new_syntaxerror(ErrorReason::InvalidLiteralNum(s))),
            }
        } else {
            match i32::from_str_radix(&s[base_len..], base).ok() {
                Some(i) => Ok(self.new_token(TokenType::IntegerLiteral(i))),
                _ => Err(self.new_syntaxerror(ErrorReason::InvalidLiteralNum(s))),
            }
        }
    }

    fn lex_string(&mut self) -> LexerResult {
        let mut s = String::new();

        while let Some(c) = self.chars.next() {
            if c == '"' {
                return Ok(self.new_token(TokenType::StringLitteral(s)))
            }
            s.push(c);
        }
        Err(self.new_syntaxerror(ErrorReason::UnterminatedString))
    }

    fn lex_char(&mut self) -> LexerResult {
        let mut s = String::new();

        while let Some(c) = self.chars.next() {
            if c == '\'' {
                let r = match (&s as &str, s.len()) {
                    ("\\0", 2) => Ok(self.new_token(TokenType::CharLiteral('\0' as i8))),
                    ("\\r", 2) => Ok(self.new_token(TokenType::CharLiteral('\r' as i8))),
                    ("\\t", 2) => Ok(self.new_token(TokenType::CharLiteral('\t' as i8))),
                    ("\\n", 2) => Ok(self.new_token(TokenType::CharLiteral('\n' as i8))),
                    (_, 1) => Ok(self.new_token(TokenType::CharLiteral(s.pop().unwrap() as i8))),
                    (_, _) => Err(self.new_syntaxerror(ErrorReason::InvalidCharLiteral(s))),
                };
                return r;
            }
            s.push(c);
        }
        Err(self.new_syntaxerror(ErrorReason::InvalidCharLiteral(s)))
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
            "import" => Ok(self.new_token(TokenType::Import)),
            "if" => Ok(self.new_token(TokenType::If)),
            "else" => Ok(self.new_token(TokenType::Else)),
            "for" => Ok(self.new_token(TokenType::For)),
            "while" => Ok(self.new_token(TokenType::While)),
            "in" => Ok(self.new_token(TokenType::In)),
            "let" => Ok(self.new_token(TokenType::Let)),
            "mut" => Ok(self.new_token(TokenType::Mut)),
            "true" => Ok(self.new_token(TokenType::True)),
            "false" => Ok(self.new_token(TokenType::False)),
            "void" => Ok(self.new_token(TokenType::Void)),
            "bool" => Ok(self.new_token(TokenType::Bool)),
            "char" => Ok(self.new_token(TokenType::Char)),
            "int" => Ok(self.new_token(TokenType::Int)),
            "double" => Ok(self.new_token(TokenType::Double)),
            _ => Ok(self.new_token(TokenType::Identifier(Rc::new(s)))),
        }
    }

    fn lex_operators(&mut self, c: char) -> LexerResult {
        match (c, self.chars.peek()) {
            ('-', Some('>')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Arrow))
            }
            ('=', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::Equal)))
            },
            ('!', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::Different)))
            },
            ('<', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::LessOrEqual)))
            },
            ('&', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::AndAssign)))
            },
            ('^', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::XorAssign)))
            },
            ('|', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::OrAssign)))
            },
            ('>', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::MoreOrEqual)))
            },
            ('>', Some('>')) => {
                self.chars.next();
                match self.chars.peek() {
                    Some('=') => {
                        self.chars.next();
                        Ok(self.new_token(TokenType::Operator(OperatorType::ShrAssign)))
                    },
                    _ => Ok(self.new_token(TokenType::Operator(OperatorType::Shr)))
                }
            },
            ('<', Some('<')) => {
                self.chars.next();
                match self.chars.peek() {
                    Some('=') => {
                        self.chars.next();
                        Ok(self.new_token(TokenType::Operator(OperatorType::ShlAssign)))
                    },
                    _ => Ok(self.new_token(TokenType::Operator(OperatorType::Shl)))
                }
            },
            ('&', Some('&')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::LogicalAnd)))
            },
            ('|', Some('|')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::LogicalOr)))
            },
            ('~', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Compl))),
            ('!', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Not))),
            ('&', _) => Ok(self.new_token(TokenType::Operator(OperatorType::And))),
            ('^', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Xor))),
            ('|', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Or))),
            ('+', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::AddAssign)))
            },
            ('-', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::SubAssign)))
            },
            ('*', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::MulAssign)))
            },
            ('/', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::DivAssign)))
            },
            ('%', Some('=')) => {
                self.chars.next();
                Ok(self.new_token(TokenType::Operator(OperatorType::RemAssign)))
            },
            ('+', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Add))),
            ('-', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Sub))),
            ('*', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Mul))),
            ('/', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Div))),
            ('%', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Rem))),
            ('<', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Less))),
            ('>', _) => Ok(self.new_token(TokenType::Operator(OperatorType::More))),
            ('=', _) => Ok(self.new_token(TokenType::Operator(OperatorType::Assign))),
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
                    '"' => Some(self.lex_string()),
                    '\'' => Some(self.lex_char()),
                    'a'...'z' | 'A'...'Z' | '_' => Some(self.lex_identifier(c)),
                    '0'...'9' => Some(self.lex_number(c)),
                    '+' | '-' | '*' | '/' | '%' | '>' | '<' | '=' | '!' | '|' | '&' | '^' | '~'
                        => Some(self.lex_operators(c)),
                    '(' => Some(Ok(self.new_token(TokenType::OpenParenthesis))),
                    ')' => Some(Ok(self.new_token(TokenType::CloseParenthesis))),
                    '{' => Some(Ok(self.new_token(TokenType::OpenBracket))),
                    '}' => Some(Ok(self.new_token(TokenType::CloseBracket))),
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
