//!
//! This module implements expressions, in all forms: variables, function calls, litteral numbers, calculations etc.
//!

use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

use iron_llvm::LLVMRef;
use iron_llvm::core::value::{RealConstRef, RealConstCtor};
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef};

use lexer::{Token, TokenType, OperatorType};
use parser::Parser;
use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRExprGenerator, IRExprResult, IRModuleProvider};
use lang::cond::{Cond, parse_cond};
use lang::Type;

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

impl IRExprGenerator for Expr {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRExprResult {
        match self.expr_type {
            ExprType::Number(n) => Ok(Type::Double.new_value(RealConstRef::get(&RealTypeRef::get_double(), n).to_ref())?), // TODO Improve Litterals
            ExprType::Variable(ref s) => match context.get_var(s) {
                Some(var) => Ok(var.value.clone_value()),
                None => Err(SyntaxError::from(&self.token, ErrorReason::UndefinedVariable(s.to_string()))),
            },
            ExprType::Unary(ref op, ref rhs) => {
                let val = rhs.gen_ir(context, module_provider)?;
                let calc = val.as_calculable();
                match op {
                    &OperatorType::Add => calc.unary_plus(context, &rhs.token),
                    &OperatorType::Sub => calc.unary_minus(context, &rhs.token),
                    _ => unimplemented!(),
                }
            },
            ExprType::Binary(ref op, ref lhs, ref rhs) => {
                use std::borrow::Borrow;

                let lhs = lhs.gen_ir(context, module_provider)?;
                let rhs = rhs.gen_ir(context, module_provider)?;
                let lhs_calc = lhs.as_calculable();
                match op {
                    &OperatorType::Add => lhs_calc.add(context, &self.token, rhs.borrow()),
                    &OperatorType::Sub => lhs_calc.sub(context, &self.token, rhs.borrow()),
                    &OperatorType::Mul => lhs_calc.mul(context, &self.token, rhs.borrow()),
                    &OperatorType::Div => lhs_calc.div(context, &self.token, rhs.borrow()),
                    &OperatorType::Rem => lhs_calc.rem(context, &self.token, rhs.borrow()),
                    &OperatorType::Less => lhs_calc.less(context, &self.token, rhs.borrow()),
                    &OperatorType::More => lhs_calc.more(context, &self.token, rhs.borrow()),
                    _ => panic!("Unimplemented binary operator \'{:?}\'.", op),
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
                        let res = arg.gen_ir(context, module_provider)?;
                        if res.get_type() != wanted_type.ty {
                            return Err(SyntaxError::from(&arg.token, ErrorReason::ArgWrongType(wanted_type.ty, res.get_type())));
                        }
                        args_value.push(res.as_llvm_ref());
                    }
                    let llvm_ref = module_provider.get_llvm_funcref_by_name(func.name.borrow() as &String).unwrap();
                    Ok(func.ret.new_value(context.builder.build_call(llvm_ref.to_ref(), args_value.as_mut_slice(), "calltmp"))?)
                } else {
                    Err(SyntaxError::from(&self.token, ErrorReason::WrongArgNumber(name.to_string(), func.args.len() as usize, args.len())))
                }
            },
            ExprType::Condition(ref cond) => cond.gen_ir(context, module_provider)
        }
    }
}
