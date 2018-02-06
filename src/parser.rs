//!
//! Koak's parser
//!

use std::collections::HashMap;
use std::iter::Peekable;
use std::slice::Iter;

use syntaxerror::{SyntaxError, ErrorReason};
use lexer::{Token, TokenType};

#[derive(Debug, Clone)]
pub enum ASTNode {
    Expr(Expr),
    ExternProto(Proto),
    Func(Proto, Expr), // Prototype, Content
}

#[derive(Debug, Clone)]
pub enum Proto {
    Prototype(String, Vec<String>), // Name, args
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    Variable(String),
    Binary(char, Box<Expr>, Box<Expr>), // Op, Exp1, Exp2
    Call(String, Vec<Expr>), // Name, args
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    last_token: Token,
    bin_ops: HashMap<char, i32>,
    file_name: String,
}

impl<'a> Parser<'a> {
    pub fn from(tokens: Peekable<Iter<'a, Token>>, vec: &Vec<Token>, file_name: &str) -> Parser<'a> {
        let last_token = vec.last().map(|x| x.clone()).unwrap_or(Token::new());
        Parser {
            tokens: tokens,
            last_token: last_token,
            bin_ops: [
                         ('<', 10),
                         ('+', 20),
                         ('-', 20),
                         ('*', 40),
            ].iter().cloned().collect(),
            file_name: file_name.to_string(),
        }
    }

    #[inline]
    fn peek_type(&mut self) -> Option<TokenType> {
        self.tokens.peek().map(|x| x.token_type.clone())
    }

    #[inline]
    fn next_or(&mut self, er: ErrorReason) -> Result<Token, SyntaxError> {
        self.tokens.next().ok_or(SyntaxError::from_token(&self.file_name, &self.last_token, er)).map(|x| x.clone())
    }

    #[inline]
    fn peek_or(&mut self, er: ErrorReason) -> Result<Token, SyntaxError> {
        self.tokens.peek().ok_or(SyntaxError::from_token(&self.file_name, &self.last_token, er)).map(|x| (*x).clone())
    }


    fn parse_call_args(&mut self) -> Result<Vec<Expr>, SyntaxError> {
        let mut v = Vec::new();

        match self.peek_type() {
            Some(TokenType::Operator(')')) => Ok(v),
            _ => {
                loop {
                    v.push(self.parse_expr()?);
                    match self.peek_type() {
                        Some(TokenType::Operator(',')) => {
                            self.tokens.next(); // eat ','
                        },
                        _ => break,
                    }
                }
                Ok(v)
            },
        }
    }

    fn parse_bin_rhs(&mut self, i: i32, lhs: Expr) -> Result<Expr, SyntaxError> {
        match self.peek_type() {
            Some(TokenType::Operator(c)) => {
                let prec = *self.bin_ops.get(&c).unwrap_or(&-1);
                if prec < i {
                    Ok(lhs)
                } else {
                    self.tokens.next(); // Eat operator
                    self.peek_or(ErrorReason::ExprExpected)?;
                    let rhs = self.parse_primary()?;

                    let rhs = {
                        match self.peek_type() {
                            Some(TokenType::Operator(c2)) => {
                                let prec_next = *self.bin_ops.get(&c2).unwrap_or(&-1);
                                if prec < prec_next {
                                    self.parse_bin_rhs(prec + 1, rhs)?
                                } else {
                                    rhs
                                }
                            },
                            _ => rhs,
                        }
                    };
                    let lhs = Expr::Binary(c, Box::new(lhs), Box::new(rhs));
                    self.parse_bin_rhs(i, lhs)
                }
            },
            _ => Ok(lhs),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, SyntaxError> {
        let t = self.next_or(ErrorReason::ExprExpected)?;
        match t.token_type {
            TokenType::Number(n) => Ok(Expr::Number(n)),
            TokenType::Operator('(') => {
                let e = self.parse_expr()?;

                let t = self.next_or(ErrorReason::UnmatchedParenthesis)?;
                match t.token_type {
                    TokenType::Operator(')') => Ok(e),
                    _ => Err(SyntaxError::from_token(&self.file_name, &t, ErrorReason::UnmatchedParenthesis)),
                }
            },
            TokenType::Identifier(s) => {
                match self.peek_type() {
                    Some(TokenType::Operator('(')) => {
                        self.tokens.next(); // Eat '('
                        let args = self.parse_call_args()?;
                        let t = self.next_or(ErrorReason::UnmatchedParenthesis)?;
                        match t.token_type {
                            TokenType::Operator(')') => Ok(Expr::Call(s, args)),
                            _ => Err(SyntaxError::from_token(&self.file_name, &t, ErrorReason::UnmatchedParenthesis)),
                        }
                    },
                    _ => Ok(Expr::Variable(s)),
                }
            },
            _ => Err(SyntaxError::from_token(&self.file_name, &t, ErrorReason::ExprExpected)),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, SyntaxError> {
        let expr = self.parse_primary()?;
        self.parse_bin_rhs(0, expr)
    }

    fn parse_prototype(&mut self) -> Result<Proto, SyntaxError> {
        let t = self.next_or(ErrorReason::ExpectedFuncName)?;
        if let TokenType::Identifier(s) = t.token_type {
            let t2 = self.next_or(ErrorReason::ExpectedOpenParenthesis)?;
            match t2.token_type {
                TokenType::Operator('(') => {

                    // Parse args
                    let mut args = Vec::new();
                    while let Some(TokenType::Identifier(s)) = self.peek_type() {
                        self.tokens.next();
                        args.push(s);
                    }

                    let t3 = self.next_or(ErrorReason::UnmatchedParenthesis)?;
                    match t3.token_type {
                        TokenType::Operator(')') => {
                            Ok(Proto::Prototype(s, args))
                        },
                        _ => Err(SyntaxError::from_token(&self.file_name, &t3, ErrorReason::ArgMustBeIdentifier))
                    }
                },
                _ => Err(SyntaxError::from_token(&self.file_name, &t2, ErrorReason::ExpectedOpenParenthesis))
            }
        } else {
            Err(SyntaxError::from_token(&self.file_name, &t, ErrorReason::ExpectedFuncName))
        }
    }

    fn parse_function_declaration(&mut self) -> Result<ASTNode, SyntaxError> {
        self.tokens.next(); // Eat def
        let proto = self.parse_prototype()?;
        let content = self.parse_expr()?;
        Ok(ASTNode::Func(proto, content))
    }

    fn parse_extern_declaration(&mut self) -> Result<ASTNode, SyntaxError> {
        self.tokens.next(); // Eat extern
        Ok(ASTNode::ExternProto(self.parse_prototype()?))
    }

    fn parse_expr_declaration(&mut self) -> Result<ASTNode, SyntaxError> {
        let expr = self.parse_expr()?;
        Ok(ASTNode::Expr(expr))
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<ASTNode, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        let r = self.peek_type()?;
        match r {
            TokenType::Def => Some(self.parse_function_declaration()),
            TokenType::Extern => Some(self.parse_extern_declaration()),
            _ => Some(self.parse_expr_declaration())
        }
    }
}
