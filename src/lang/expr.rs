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
use codegen::{IRContext, IRExprGenerator, IRExprResult, IRModuleProvider};
use lang::types::KoakType;
use lang::value::KoakValue;

lazy_static! {
    static ref BIN_OPS: HashMap<OperatorType, i32> = [
        (OperatorType::Assign, 10),
        (OperatorType::AddAssign, 10),
        (OperatorType::SubAssign, 10),
        (OperatorType::MulAssign, 10),
        (OperatorType::DivAssign, 10),
        (OperatorType::RemAssign, 10),
        (OperatorType::XorAssign, 10),
        (OperatorType::AndAssign, 10),
        (OperatorType::OrAssign, 10),
        (OperatorType::ShrAssign, 10),
        (OperatorType::ShlAssign, 10),
        (OperatorType::LogicalAnd, 20),
        (OperatorType::LogicalOr, 30),
        (OperatorType::Or, 40),
        (OperatorType::Xor, 50),
        (OperatorType::Equal, 70),
        (OperatorType::And, 60),
        (OperatorType::Different, 70),
        (OperatorType::Less, 80),
        (OperatorType::More, 80),
        (OperatorType::LessOrEqual, 80),
        (OperatorType::MoreOrEqual, 80),
        (OperatorType::Shl, 90),
        (OperatorType::Shr, 90),
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
        _ => Err(SyntaxError::from(&expr, ErrorReason::ExprExpected)),
    }
}

fn parse_unary(parser: &mut Parser) -> Result<Expr, SyntaxError> {
    match parser.peek_type() {
        Some(&TokenType::Operator(t @ OperatorType::Add))
        | Some(&TokenType::Operator(t @ OperatorType::Sub))
        | Some(&TokenType::Operator(t @ OperatorType::Not))
        | Some(&TokenType::Operator(t @ OperatorType::Compl))
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
            ExprType::BoolLiteral(b) => Ok(KoakValue::new(IntConstRef::get(&IntTypeRef::get_int1(), b as u64, true).to_ref(), KoakType::Bool)),
            ExprType::CharLiteral(c) => Ok(KoakValue::new(IntConstRef::get(&IntTypeRef::get_int8(), c as u64, true).to_ref(), KoakType::Char)),
            ExprType::IntegerLiteral(n) => Ok(KoakValue::new(IntConstRef::get(&IntTypeRef::get_int32(), n as u64, true).to_ref(), KoakType::Int)),
            ExprType::DoubleLiteral(n) => Ok(KoakValue::new(RealConstRef::get(&RealTypeRef::get_double(), n).to_ref(), KoakType::Double)),
            ExprType::Variable(ref s) => match context.get_local_var(s) {
                Some(var) => Ok(var),
                None => Err(SyntaxError::from(&self.token, ErrorReason::UndefinedVariable(s.to_string()))),
            },
            ExprType::Unary(ref op, ref expr) => {
                let val = expr.gen_ir(context, module_provider)?;
                match *op {
                    OperatorType::Add => Ok(val),
                    OperatorType::Sub => val.unary_neg(context, &expr.token), // -
                    OperatorType::Compl => val.unary_compl(context, &expr.token), // ~
                    OperatorType::Not => val.unary_not(context, &expr.token), // !
                    _ => unreachable!(),
                }
            },
            ExprType::Binary(ref op, ref lhs, ref rhs) => {
                let mut lhs = lhs.gen_ir(context, module_provider)?;
                if *op == OperatorType::LogicalAnd {
                    lhs.logical_and(context, module_provider, &self.token, rhs)
                } else if *op == OperatorType::LogicalOr {
                    lhs.logical_or(context, module_provider, &self.token, rhs)
                } else {
                    let rhs = rhs.gen_ir(context, module_provider)?;
                    match *op {
                        OperatorType::MoreOrEqual => lhs.ge(context, &self.token, rhs), // >=
                        OperatorType::LessOrEqual => lhs.le(context, &self.token, rhs), // <=
                        OperatorType::Equal => lhs.eq(context, &self.token, rhs), // ==
                        OperatorType::Different => lhs.ne(context, &self.token, rhs), // !=
                        OperatorType::Shr => lhs.shr(context, &self.token, rhs), // >>
                        OperatorType::Shl => lhs.shl(context, &self.token, rhs), // <<
                        OperatorType::And => lhs.bitwise_and(context, &self.token, rhs), // &
                        OperatorType::Or => lhs.bitwise_or(context, &self.token, rhs), // |
                        OperatorType::Xor => lhs.bitwise_xor(context, &self.token, rhs), // ^
                        OperatorType::Add => lhs.add(context, &self.token, rhs), // +
                        OperatorType::Sub => lhs.sub(context, &self.token, rhs), // -
                        OperatorType::Mul => lhs.mul(context, &self.token, rhs), // *
                        OperatorType::Div => lhs.div(context, &self.token, rhs), // /
                        OperatorType::Rem => lhs.rem(context, &self.token, rhs), // %
                        OperatorType::Less => lhs.lt(context, &self.token, rhs), // <
                        OperatorType::More => lhs.gt(context, &self.token, rhs), // >
                        OperatorType::Assign => { lhs.assign(context, &self.token, rhs)?; Ok(lhs) },
                        OperatorType::AddAssign => { lhs.add_assign(context, &self.token, rhs)?; Ok(lhs) },
                        OperatorType::SubAssign => { lhs.sub_assign(context, &self.token, rhs)?; Ok(lhs) },
                        OperatorType::MulAssign => { lhs.mul_assign(context, &self.token, rhs)?; Ok(lhs) },
                        OperatorType::DivAssign => { lhs.div_assign(context, &self.token, rhs)?; Ok(lhs) },
                        OperatorType::RemAssign => { lhs.rem_assign(context, &self.token, rhs)?; Ok(lhs) },
                        OperatorType::XorAssign => { lhs.xor_assign(context, &self.token, rhs)?; Ok(lhs) },
                        OperatorType::AndAssign => { lhs.and_assign(context, &self.token, rhs)?; Ok(lhs) },
                        OperatorType::OrAssign => { lhs.or_assign(context, &self.token, rhs)?; Ok(lhs) },
                        OperatorType::ShrAssign => { lhs.shr_assign(context, &self.token, rhs)?; Ok(lhs) },
                        OperatorType::ShlAssign => { lhs.shl_assign(context, &self.token, rhs)?; Ok(lhs) },
                        _ => unreachable!(),
                    }
                }
            }
            ExprType::Call(ref name, ref args) => {
                // Get the prototype of the function corresponding to the given name
                let func = (context.functions.get(name).ok_or_else(|| SyntaxError::from(&self.token, ErrorReason::UndefinedFunction(name.to_string())))?).clone();

                // Validate arguments
                if args.len() == func.args.len() as usize {
                    let mut args_value = Vec::new();
                    for (arg, wanted_type) in args.iter().zip(func.args.iter()) {
                        let arg_val = arg.gen_ir(context, module_provider)?;
                        let arg_casted = arg_val.cast_to(&arg.token, context, wanted_type.ty)?;
                        args_value.push(arg_casted.llvm_ref);
                    }
                    let llvm_ref = module_provider.get_llvm_funcref_by_name(&func.name as &String).unwrap().0;
                    if let KoakType::Void = func.ret_ty {
                        context.builder.build_call(llvm_ref.to_ref(), args_value.as_mut_slice(), ""); // Void function call must have an anonymous name
                        Ok(KoakValue::new_void())

                    } else {
                        Ok(KoakValue::new(context.builder.build_call(llvm_ref.to_ref(), args_value.as_mut_slice(), "calltmp"), func.ret_ty))
                    }
                } else {
                    Err(SyntaxError::from(&self.token, ErrorReason::WrongArgNumber(name.to_string(), func.args.len() as usize, args.len())))
                }
            },
        }
    }
}
