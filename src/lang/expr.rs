//!
//! This module implements expressions, in all forms: variables, function calls, Literal numbers, calculations etc.
//!

use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

use iron_llvm::LLVMRef;
use iron_llvm::core::types::{IntTypeRef, IntTypeCtor, RealTypeRef, RealTypeCtor};
use iron_llvm::core::value::{IntConstRef, IntConstCtor, RealConstRef, RealConstCtor};

use lexer::{Token, TokenType, OperatorType};
use parser::Parser;
use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRGenerator, IRResult, IRModuleProvider};
use lang::cond::{Cond, parse_cond};
use lang::types::KoakCalculable;
use lang::types;

lazy_static! {
    static ref BIN_OPS: HashMap<OperatorType, i32> = [
        (OperatorType::Less, 80),
        (OperatorType::More, 80),
        (OperatorType::Add, 100),
        (OperatorType::Sub, 100),
        (OperatorType::Mul, 110),
        (OperatorType::Div, 110),
        (OperatorType::Rem, 110),
    ].iter().cloned().collect();
}

#[derive(Debug, Clone)]
pub enum ExprType {
    BoolLiteral(bool),
    CharLiteral(i8),
    IntegerLiteral(i32),
    DoubleLiteral(f64),
    Variable(Rc<String>),
    Unary(OperatorType, Box<Expr>),
    Binary(OperatorType, Box<Expr>, Box<Expr>), // Op, Exp1, Exp2
    Call(Rc<String>, Vec<Expr>), // Name, args
    Condition(Box<Cond>),
}

#[derive(Clone)]
pub struct Expr {
    pub token: Token,
    pub expr_type: ExprType,
}

impl Expr {
    #[inline]
    pub fn new(token: Token, t: ExprType) -> Expr {
        Expr {
            token: token,
            expr_type: t,
        }
    }
}

impl fmt::Debug for Expr {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.expr_type)
    }
}

fn parse_call_args(parser: &mut Parser) -> Result<Vec<Expr>, SyntaxError> {
    let mut v = Vec::new();

    match parser.peek_type() {
        Some(&TokenType::CloseParenthesis) => Ok(v),
        _ => {
            loop {
                v.push(parse_expr(parser)?);
                match parser.peek_type() {
                    Some(&TokenType::Comma) => { parser.tokens.pop(); } , // eat ','
                    _ => break,
                }
            }
            Ok(v)
        },
    }
}

fn parse_bin_rhs(parser: &mut Parser, i: i32, lhs: Expr) -> Result<Expr, SyntaxError> {
    match parser.peek_type() {
        Some(&TokenType::Operator(c)) => {
            let prec = *BIN_OPS.get(&c).unwrap_or(&-1);
            if prec < i {
                Ok(lhs)
            } else {
                let op = parser.tokens.pop().unwrap(); // Eat operator
                parser.peek_or(ErrorReason::ExprExpected)?;
                let rhs = parse_unary(parser)?;

                let rhs = {
                    match parser.peek_type() {
                        Some(&TokenType::Operator(c2)) => {
                            let prec_next = *BIN_OPS.get(&c2).unwrap_or(&-1);
                            if prec < prec_next {
                                parse_bin_rhs(parser, prec + 1, rhs)?
                            } else {
                                rhs
                            }
                        },
                        _ => rhs,
                    }
                };
                let lhs = Expr::new(op, ExprType::Binary(c, Box::new(lhs), Box::new(rhs)));
                parse_bin_rhs(parser, i, lhs)
            }
        },
        _ => Ok(lhs),
    }
}

fn parse_primary(parser: &mut Parser) -> Result<Expr, SyntaxError> {
    let expr = parser.next_or(ErrorReason::ExprExpected)?;
    match expr.token_type {
        TokenType::True => Ok(Expr::new(expr, ExprType::BoolLiteral(true))),
        TokenType::False => Ok(Expr::new(expr, ExprType::BoolLiteral(false))),
        TokenType::CharLiteral(c) => Ok(Expr::new(expr, ExprType::CharLiteral(c))),
        TokenType::DoubleLiteral(n) => Ok(Expr::new(expr, ExprType::DoubleLiteral(n))),
        TokenType::IntegerLiteral(n) => Ok(Expr::new(expr, ExprType::IntegerLiteral(n))),
        TokenType::OpenParenthesis => {
            let expr = parse_expr(parser)?;

            let close = parser.next_or(ErrorReason::UnmatchedParenthesis)?;
            match close.token_type {
                TokenType::CloseParenthesis => Ok(expr),
                _ => Err(SyntaxError::from(&close, ErrorReason::UnmatchedParenthesis)),
            }
        },
        TokenType::Identifier(_) => {
            let identifier = if let TokenType::Identifier(ref s) = expr.token_type { s.clone() } else { unreachable!() };
            match parser.peek_type() {
                Some(&TokenType::OpenParenthesis) => { // Function call ?
                    parser.tokens.pop(); // Eat '('
                    let args = parse_call_args(parser)?;
                    let close = parser.next_or(ErrorReason::UnmatchedParenthesis)?;
                    match close.token_type {
                        TokenType::CloseParenthesis => Ok(Expr::new(expr, ExprType::Call(identifier, args))),
                        _ => Err(SyntaxError::from(&close, ErrorReason::UnmatchedParenthesis)),
                    }
                },
                _ => Ok(Expr::new(expr, ExprType::Variable(identifier))),
            }
        },
        TokenType::If => Ok(Expr::new(expr, ExprType::Condition(Box::new(parse_cond(parser)?)))),
        _ => Err(SyntaxError::from(&expr, ErrorReason::ExprExpected)),
    }
}

fn parse_unary(parser: &mut Parser) -> Result<Expr, SyntaxError> {
    match parser.peek_type() {
        Some(&TokenType::Operator(t @ OperatorType::Add))
            | Some(&TokenType::Operator(t @ OperatorType::Sub))
                => {
                    let token = parser.tokens.pop().unwrap();
                    Ok(Expr::new(token, ExprType::Unary(t, Box::new(parse_unary(parser)?))))
                },
        _ => parse_primary(parser),
    }
}

#[inline]
pub fn parse_expr(parser: &mut Parser) -> Result<Expr, SyntaxError> {
    let expr = parse_unary(parser)?;
    parse_bin_rhs(parser, 0, expr)
}

impl IRGenerator for Expr {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRResult {
        match self.expr_type {
            ExprType::BoolLiteral(b) => Ok(IntConstRef::get(&IntTypeRef::get_int1(), b as u64, true).to_ref()),
            ExprType::CharLiteral(c) => Ok(IntConstRef::get(&IntTypeRef::get_int8(), c as u64, true).to_ref()),
            ExprType::IntegerLiteral(n) => Ok(IntConstRef::get(&IntTypeRef::get_int32(), n as u64, true).to_ref()),
            ExprType::DoubleLiteral(n) => Ok(RealConstRef::get(&RealTypeRef::get_double(), n).to_ref()),
            ExprType::Variable(ref s) => match context.get_var(s) {
                Some(var) => Ok(var),
                None => Err(SyntaxError::from(&self.token, ErrorReason::UndefinedVariable(s.to_string()))),
            },
            ExprType::Unary(ref op, ref expr) => {
                let val = expr.gen_ir(context, module_provider)?;
                match op {
                    &OperatorType::Add => Ok(val),
                    &OperatorType::Sub => val.unary_not(context, &expr.token),
                    _ => unimplemented!(),
                }
            },
            ExprType::Binary(ref op, ref lhs, ref rhs) => {
                let lhs = lhs.gen_ir(context, module_provider)?;
                let rhs = rhs.gen_ir(context, module_provider)?;
                match op {
                    &OperatorType::Add => KoakCalculable::add(&lhs, context, &self.token, rhs),
                    &OperatorType::Sub => KoakCalculable::sub(&lhs, context, &self.token, rhs),
                    &OperatorType::Mul => KoakCalculable::mul(&lhs, context, &self.token, rhs),
                    &OperatorType::Div => KoakCalculable::div(&lhs, context, &self.token, rhs),
                    &OperatorType::Rem => KoakCalculable::rem(&lhs, context, &self.token, rhs),
                    &OperatorType::Less => KoakCalculable::lt(&lhs, context, &self.token, rhs),
                    &OperatorType::More => KoakCalculable::gt(&lhs, context, &self.token, rhs),
                    _ => unimplemented!(),
                }
            }
            ExprType::Call(ref name, ref args) => {
                use std::borrow::Borrow;

                // Get the abstract function corresponding to the given name
                let func = (context.functions.get(name).ok_or(SyntaxError::from(&self.token, ErrorReason::UndefinedFunction(name.to_string())))?).clone();

                // Validate arguments
                if args.len() == func.args.len() as usize {
                    let mut args_value = Vec::new();
                    for (arg, wanted_type) in args.iter().zip(func.args.iter()) {
                        let arg_val = arg.gen_ir(context, module_provider)?;
                        let cast_arg = types::cast_to(arg_val, wanted_type.ty, context)?;
                        args_value.push(cast_arg);
                    }
                    let llvm_ref = module_provider.get_llvm_funcref_by_name(func.name.borrow() as &String).unwrap();
                    Ok(context.builder.build_call(llvm_ref.to_ref(), args_value.as_mut_slice(), "calltmp"))
                } else {
                    Err(SyntaxError::from(&self.token, ErrorReason::WrongArgNumber(name.to_string(), func.args.len() as usize, args.len())))
                }
            },
            ExprType::Condition(ref cond) => cond.gen_ir(context, module_provider)
        }
    }
}
