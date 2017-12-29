//!
//! Koak's parser
//!

use std::collections::HashMap;

use syntaxerror::{SyntaxError, ErrorReason};
use token::TokenType;
use lexer::Lexer;

#[derive(Debug)]
pub enum ASTNode {
    Expr(Expr),
    Proto(Proto),
    Func(Func),
}

#[derive(Debug)]
pub enum Func {
    Function(Proto, Expr),
}

#[derive(Debug)]
pub enum Proto {
    Prototype(String, Vec<String>),
}

#[derive(Debug)]
pub enum Expr {
    Number(f64),
    Variable(String),
    Binary(char, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug)]
pub struct Parser {
    bin_ops: HashMap<char, i32>
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            bin_ops: [
                         ('<', 10),
                         ('+', 20),
                         ('-', 20),
                         ('*', 40),
            ].iter().cloned().collect(),
        }
    }

    fn parse_call_args(&mut self, lexer: &mut Lexer) -> Result<Vec<Expr>, SyntaxError> {
        let mut v = Vec::new();

        match lexer.peek_type() {
            Some(Ok(TokenType::Operator(')'))) => Ok(v),
            _ => {
                loop {
                    v.push(self.parse_expr(lexer)?);
                    match lexer.peek_type() {
                        Some(Ok(TokenType::Operator(','))) => {
                            lexer.next(); // eat ','
                        },
                        _ => break,
                    }
                }
                Ok(v)
            },
        }
    }

    fn parse_bin_rhs(&mut self, lexer: &mut Lexer, i: i32, lhs: Expr) -> Result<Expr, SyntaxError> {
        match lexer.peek_type() {
            Some(Ok(TokenType::Operator(c))) => {
                let prec = *self.bin_ops.get(&c).unwrap_or(&-1);
                if prec < i {
                    Ok(lhs)
                } else {
                    lexer.next(); // Eat operator
                    let rhs = self.parse_primary(lexer)?;

                    let rhs = {
                        match lexer.peek_type() {
                            Some(Ok(TokenType::Operator(c2))) => {
                                let prec_next = *self.bin_ops.get(&c2).unwrap_or(&-1);
                                if prec < prec_next {
                                    self.parse_bin_rhs(lexer, prec + 1, rhs)?
                                } else {
                                    rhs
                                }
                            },
                            _ => rhs,
                        }
                    };
                    let lhs = Expr::Binary(c, Box::new(lhs), Box::new(rhs));
                    self.parse_bin_rhs(lexer, i, lhs)
                }
            },
            _ => Ok(lhs),
        }
    }

    fn parse_primary(&mut self, lexer: &mut Lexer) -> Result<Expr, SyntaxError> {
        let t = lexer.next_or(ErrorReason::ExprExpected)?;
        match t.get_type() {
            TokenType::Number(n) => Ok(Expr::Number(n)),
            TokenType::Operator('(') => {
                let e = self.parse_expr(lexer)?;

                let t = lexer.next_or(ErrorReason::UnmatchedParenthesis)?;
                match t.get_type() {
                    TokenType::Operator(')') => Ok(e),
                    _ => Err(SyntaxError::from(lexer, &t, ErrorReason::UnmatchedParenthesis)),
                }
            },
            TokenType::Identifier(s) => {
                match lexer.peek_type() {
                    Some(Ok(TokenType::Operator('('))) => {
                        lexer.next(); // Eat '('
                        let args = self.parse_call_args(lexer)?;
                        let t = lexer.next_or(ErrorReason::UnmatchedParenthesis)?;
                        match t.get_type() {
                            TokenType::Operator(')') => Ok(Expr::Call(s, args)),
                            _ => Err(SyntaxError::from(lexer, &t, ErrorReason::UnmatchedParenthesis)),
                        }
                    },
                    _ => Ok(Expr::Variable(s)),
                }
            },
            _ => Err(SyntaxError::from(lexer, &t, ErrorReason::ExprExpected)),
        }
    }

    fn parse_expr(&mut self, lexer: &mut Lexer) -> Result<Expr, SyntaxError> {
        let expr = self.parse_primary(lexer)?;
        self.parse_bin_rhs(lexer, 0, expr)
    }

    fn parse_prototype(&mut self, lexer: &mut Lexer) -> Result<Proto, SyntaxError> {
        let t = lexer.next_or(ErrorReason::ExpectedFuncName)?;
        if let TokenType::Identifier(s) = t.get_type() {
            let t2 = lexer.next_or(ErrorReason::ExpectedOpenParenthesis)?;
            match t2.get_type() {
                TokenType::Operator('(') => {
                    let mut args = Vec::new();
                    // Parse args
                    while let Some(Ok(TokenType::Identifier(s))) = lexer.peek_type() {
                        lexer.next();
                        args.push(s);
                    }

                    let t3 = lexer.next_or(ErrorReason::UnmatchedParenthesis)?;
                    match t3.get_type() {
                        TokenType::Operator(')') => {
                            Ok(Proto::Prototype(s, args))
                        },
                        _ => Err(SyntaxError::from(lexer, &t3, ErrorReason::ArgMustBeIdentifier))
                    }
                },
                _ => Err(SyntaxError::from(lexer, &t2, ErrorReason::ExpectedOpenParenthesis))
            }
        } else {
            Err(SyntaxError::from(lexer, &t, ErrorReason::ExpectedFuncName))
        }
    }

    pub fn parse(&mut self, lexer: &mut Lexer) -> Result<(), SyntaxError> {
        while let Some(r) = lexer.peek_type() {
            let r = r?;
            let node = match r {
                TokenType::Def => {
                    lexer.next(); // Eat 'def'
                    ASTNode::Proto(self.parse_prototype(lexer)?)
                },
                _ => ASTNode::Expr(self.parse_expr(lexer)?),
            };
            println!("Node: {:?}", node);
        }
        Ok(())
    }
}
