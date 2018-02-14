//!
//! This module implements expressions, in all forms: variables, function calls, litteral numbers, calculations etc.
//!

use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

use llvm_sys::LLVMOpcode;
use llvm_sys::LLVMRealPredicate::LLVMRealOLT;

use iron_llvm::LLVMRef;
use iron_llvm::core::value::{Function, RealConstRef, RealConstCtor};

use lexer::{Token, TokenType, OperatorType};
use parser::Parser;
use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRGenerator, IRResult, IRModuleProvider};

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
    Number(f64),
    Variable(Rc<String>),
    Unary(OperatorType, Box<Expr>),
    Binary(OperatorType, Box<Expr>, Box<Expr>), // Op, Exp1, Exp2
    Call(Rc<String>, Vec<Expr>), // Name, args
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
        TokenType::Number(n) => Ok(Expr::new(expr, ExprType::Number(n))),
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
        use std::borrow::Borrow;
        match self.expr_type {
            ExprType::Number(n) => Ok(RealConstRef::get(&context.double_type, n).to_ref()),
            ExprType::Variable(ref s) => match context.named_values.get(s.borrow() as &String) {
                Some(value) => Ok(*value),
                None => Err(SyntaxError::from(&self.token, ErrorReason::UndefinedVariable(s.to_string())))
            },
            ExprType::Unary(ref op, ref rhs) => {
                match op {
                    &OperatorType::Add => rhs.gen_ir(context, module_provider),
                    &OperatorType::Sub => {
                        let rhs = rhs.gen_ir(context, module_provider)?;
                        Ok(context.builder.build_fneg(rhs, "unaryneg"))
                    }
                    _ => unimplemented!(),
                }
            },
            ExprType::Binary(ref op, ref lhs, ref rhs) => {
                let lhs = lhs.gen_ir(context, module_provider)?;
                let rhs = rhs.gen_ir(context, module_provider)?;
                match op {
                    &OperatorType::Add => Ok(context.builder.build_fadd(lhs, rhs, "addtmp")),
                    &OperatorType::Sub => Ok(context.builder.build_fsub(lhs, rhs, "subtmp")),
                    &OperatorType::Mul => Ok(context.builder.build_fmul(lhs, rhs, "mulmp")),
                    &OperatorType::Div => Ok(context.builder.build_fdiv(lhs, rhs, "divtmp")),
                    &OperatorType::Rem => Ok(context.builder.build_frem(lhs, rhs, "modtmp")),
                    &OperatorType::Less => {
                        let tmp = context.builder.build_fcmp(LLVMRealOLT, lhs, rhs, "cmptmp");
                        Ok(context.builder.build_cast(LLVMOpcode::LLVMUIToFP, tmp, context.double_type.to_ref(), "casttmp"))
                    },
                    &OperatorType::More => {
                        let tmp = context.builder.build_fcmp(LLVMRealOLT, rhs, lhs, "cmptmp");
                        Ok(context.builder.build_cast(LLVMOpcode::LLVMUIToFP, tmp, context.double_type.to_ref(), "casttmp"))
                    },
                    _ => panic!("Unimplemented binary operator \'{:?}\'.", op),
                }
            }
            ExprType::Call(ref name, ref args) => {
                let func = module_provider.get_function(name).ok_or(SyntaxError::from(&self.token, ErrorReason::UndefinedFunction(name.to_string())))?;
                if args.len() == func.count_params() as usize {
                    let mut args_value = Vec::new();
                    for arg in args.iter() {
                        args_value.push(arg.gen_ir(context, module_provider)?);
                    }
                    Ok(context.builder.build_call(func.to_ref(), args_value.as_mut_slice(), "calltmp"))
                } else {
                    Err(SyntaxError::from(&self.token, ErrorReason::WrongArgNumber(name.to_string(), func.count_params() as usize, args.len())))
                }
            },
        }
    }
}
