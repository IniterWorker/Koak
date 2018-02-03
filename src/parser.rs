//!
//! Koak's parser
//!

use std::collections::HashMap;

use syntaxerror::{SyntaxError, ErrorReason};
use token::TokenType;
use lexer::Lexer;

#[derive(Debug, Clone)]
pub enum ASTNode {
    Expr(Expr),
    ExternProto(Proto),
    Func(Proto, Expr),
}

#[derive(Debug, Clone)]
pub enum Proto {
    Prototype(String, Vec<String>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    Variable(String),
    Binary(char, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    peeked: Option<Option<Result<ASTNode, SyntaxError>>>,
    bin_ops: HashMap<char, i32>
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer: lexer,
            peeked: None,
            bin_ops: [
                         ('<', 10),
                         ('+', 20),
                         ('-', 20),
                         ('*', 40),
            ].iter().cloned().collect(),
        }
    }

    fn parse_call_args(&mut self) -> Result<Vec<Expr>, SyntaxError> {
        let mut v = Vec::new();

        match self.lexer.peek_type() {
            Some(Ok(TokenType::Operator(')'))) => Ok(v),
            _ => {
                loop {
                    v.push(self.parse_expr()?);
                    match self.lexer.peek_type() {
                        Some(Ok(TokenType::Operator(','))) => {
                            self.lexer.next(); // eat ','
                        },
                        _ => break,
                    }
                }
                Ok(v)
            },
        }
    }

    fn parse_bin_rhs(&mut self, i: i32, lhs: Expr) -> Result<Expr, SyntaxError> {
        match self.lexer.peek_type() {
            Some(Ok(TokenType::Operator(c))) => {
                let prec = *self.bin_ops.get(&c).unwrap_or(&-1);
                if prec < i {
                    Ok(lhs)
                } else {
                    self.lexer.next(); // Eat operator
                    let rhs = self.parse_primary()?;

                    let rhs = {
                        match self.lexer.peek_type() {
                            Some(Ok(TokenType::Operator(c2))) => {
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
        let t = self.lexer.next_or(ErrorReason::ExprExpected)?;
        match t.get_type() {
            TokenType::Number(n) => Ok(Expr::Number(n)),
            TokenType::Operator('(') => {
                let e = self.parse_expr()?;

                let t = self.lexer.next_or(ErrorReason::UnmatchedParenthesis)?;
                match t.get_type() {
                    TokenType::Operator(')') => Ok(e),
                    _ => Err(SyntaxError::from(&self.lexer, &t, ErrorReason::UnmatchedParenthesis)),
                }
            },
            TokenType::Identifier(s) => {
                match self.lexer.peek_type() {
                    Some(Ok(TokenType::Operator('('))) => {
                        self.lexer.next(); // Eat '('
                        let args = self.parse_call_args()?;
                        let t = self.lexer.next_or(ErrorReason::UnmatchedParenthesis)?;
                        match t.get_type() {
                            TokenType::Operator(')') => Ok(Expr::Call(s, args)),
                            _ => Err(SyntaxError::from(&self.lexer, &t, ErrorReason::UnmatchedParenthesis)),
                        }
                    },
                    _ => Ok(Expr::Variable(s)),
                }
            },
            _ => Err(SyntaxError::from(&self.lexer, &t, ErrorReason::ExprExpected)),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, SyntaxError> {
        let expr = self.parse_primary()?;
        self.parse_bin_rhs(0, expr)
    }

    fn parse_prototype(&mut self) -> Result<Proto, SyntaxError> {
        let t = self.lexer.next_or(ErrorReason::ExpectedFuncName)?;
        if let TokenType::Identifier(s) = t.get_type() {
            let t2 = self.lexer.next_or(ErrorReason::ExpectedOpenParenthesis)?;
            match t2.get_type() {
                TokenType::Operator('(') => {
                    let mut args = Vec::new();
                    // Parse args
                    while let Some(Ok(TokenType::Identifier(s))) = self.lexer.peek_type() {
                        self.lexer.next();
                        args.push(s);
                    }

                    let t3 = self.lexer.next_or(ErrorReason::UnmatchedParenthesis)?;
                    match t3.get_type() {
                        TokenType::Operator(')') => {
                            Ok(Proto::Prototype(s, args))
                        },
                        _ => Err(SyntaxError::from(&self.lexer, &t3, ErrorReason::ArgMustBeIdentifier))
                    }
                },
                _ => Err(SyntaxError::from(&self.lexer, &t2, ErrorReason::ExpectedOpenParenthesis))
            }
        } else {
            Err(SyntaxError::from(&self.lexer, &t, ErrorReason::ExpectedFuncName))
        }
    }

    pub fn parse_function(&mut self) -> Result<ASTNode, SyntaxError> {
        self.lexer.next(); // Eat def
        let proto = self.parse_prototype()?;
        let content = self.parse_expr()?;
        Ok(ASTNode::Func(proto, content))
    }

    pub fn parse_extern_declaration(&mut self) -> Result<ASTNode, SyntaxError> {
        self.lexer.next(); // Eat extern
        Ok(ASTNode::ExternProto(self.parse_prototype()?))
    }

    #[allow(dead_code)]
    pub fn get_lexer(&self) -> &Lexer {
        &self.lexer
    }

    #[allow(dead_code)]
    pub fn get_lexer_mut(&mut self) -> &mut Lexer {
        &mut self.lexer
    }

    pub fn peek(&mut self) -> Option<Result<ASTNode, SyntaxError>> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next());
        }
        match self.peeked {
            Some(Some(ref value)) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn next_node(&mut self) -> Result<ASTNode, SyntaxError> {
        let r = self.lexer.peek_type().unwrap()?;
        let node = match r {
                TokenType::Def => self.parse_function()?,
                TokenType::Extern => self.parse_extern_declaration()?,
                _ => ASTNode::Expr(self.parse_expr()?),
        };
        Ok(node)
    }
}

impl Iterator for Parser {
    type Item = Result<ASTNode, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(v) => v,
            None => self.lexer.peek_type().and_then(|_| Some(self.next_node())),
        }
    }
}
