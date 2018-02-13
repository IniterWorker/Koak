//!
//! This module implements expressions, in all forms: variables, function calls, litteral numbers, calculations etc.
//!

use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

use llvm_sys::LLVMRealPredicate::LLVMRealOLT;

use iron_llvm::LLVMRef;
use iron_llvm::core::value::{Function, RealConstRef, RealConstCtor};

use lexer::{Token, TokenType};
use parser::Parser;
use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRGenerator, IRResult, IRModuleProvider};

lazy_static! {
    static ref BIN_OPS: HashMap<char, i32> = [
        ('<', 10),
        ('>', 10),
        ('+', 20),
        ('-', 20),
        ('*', 40),
        ('/', 40),
        ('%', 40),
    ].iter().cloned().collect();
}

#[derive(Debug, Clone)]
pub enum ExprType {
    Number(f64),
    Variable(Rc<String>),
    Binary(char, Box<Expr>, Box<Expr>), // Op, Exp1, Exp2
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
        Some(&TokenType::Operator(')')) => Ok(v),
        _ => {
            loop {
                v.push(parse_expr(parser)?);
                match parser.peek_type() {
                    Some(&TokenType::Operator(',')) => {
                        parser.tokens.pop(); // eat ','
                    },
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
                let rhs = parse_primary(parser)?;

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
        TokenType::Operator('(') => {
            let expr = parse_expr(parser)?;

            let close = parser.next_or(ErrorReason::UnmatchedParenthesis)?;
            match close.token_type {
                TokenType::Operator(')') => Ok(expr),
                _ => Err(SyntaxError::from(&close, ErrorReason::UnmatchedParenthesis)),
            }
        },
        TokenType::Identifier(_) => {
            let identifier = if let TokenType::Identifier(ref s) = expr.token_type { s.clone() } else { unreachable!() };
            match parser.peek_type() {
                Some(&TokenType::Operator('(')) => { // Function call ?
                    parser.tokens.pop(); // Eat '('
                    let args = parse_call_args(parser)?;
                    let close = parser.next_or(ErrorReason::UnmatchedParenthesis)?;
                    match close.token_type {
                        TokenType::Operator(')') => Ok(Expr::new(expr, ExprType::Call(identifier, args))),
                        _ => Err(SyntaxError::from(&close, ErrorReason::UnmatchedParenthesis)),
                    }
                },
                _ => Ok(Expr::new(expr, ExprType::Variable(identifier))),
            }
        },
        _ => Err(SyntaxError::from(&expr, ErrorReason::ExprExpected)),
    }
}

#[inline]
pub fn parse_expr(parser: &mut Parser) -> Result<Expr, SyntaxError> {
    let expr = parse_primary(parser)?;
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
            ExprType::Binary(op, ref lhs, ref rhs) => {
                let lhs = lhs.gen_ir(context, module_provider)?;
                let rhs = rhs.gen_ir(context, module_provider)?;
                match op {
                    '+' => Ok(context.builder.build_fadd(lhs, rhs, "addtmp")),
                    '-' => Ok(context.builder.build_fsub(lhs, rhs, "subtmp")),
                    '*' => Ok(context.builder.build_fmul(lhs, rhs, "mulmp")),
                    '/' => Ok(context.builder.build_fdiv(lhs, rhs, "divtmp")),
                    '%' => Ok(context.builder.build_frem(lhs, rhs, "modtmp")),
                    '<' => Ok(context.builder.build_fcmp(LLVMRealOLT, lhs, rhs, "cmptmp")),
                    '>' => Ok(context.builder.build_fcmp(LLVMRealOLT, rhs, lhs, "cmptmp")),
                    _ => panic!("Unexpected binary operator \'{}\'.", op),
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
