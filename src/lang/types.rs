//!
//! Koak's type system.
//!
//!

use std::fmt;

use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};
use llvm_sys::prelude::{LLVMTypeRef};

use iron_llvm::{LLVMRef};
use iron_llvm::core::value::{RealConstRef, IntConstRef, RealConstCtor, IntConstCtor};
use iron_llvm::core::types::{IntTypeRef, IntTypeCtor, RealTypeRef, RealTypeCtor, VoidTypeRef, VoidTypeCtor};

use error::{ErrorReason, SyntaxError};
use codegen::{IRContext, IRExprResult};
use lexer::{Token, TokenType};
use lang::value::KoakValue;

pub enum KoakTypeKind {
    Integer,
    UnsignedInteger,
    FloatingPoint,
    Other,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum KoakType {
    Void,
    Bool,
    Char,
    Int,
    Double,
}

impl KoakType {
    #[inline]
    pub fn as_llvm_ref(&self) -> LLVMTypeRef {
        match *self {
            KoakType::Void => VoidTypeRef::get().to_ref(),
            KoakType::Bool => IntTypeRef::get_int1().to_ref(),
            KoakType::Char => IntTypeRef::get_int8().to_ref(),
            KoakType::Int => IntTypeRef::get_int32().to_ref(),
            KoakType::Double => RealTypeRef::get_double().to_ref(),
        }
    }

    #[inline]
    pub fn from(token: &Token) -> Result<KoakType, SyntaxError> {
        match token.token_type {
            TokenType::Void => Ok(KoakType::Void),
            TokenType::Bool => Ok(KoakType::Bool),
            TokenType::Char => Ok(KoakType::Char),
            TokenType::Int => Ok(KoakType::Int),
            TokenType::Double => Ok(KoakType::Double),
            _ => Err(SyntaxError::from(token, ErrorReason::InvalidType)),
        }
    }

    pub fn get_kind(&self) -> KoakTypeKind {
        match *self {
            KoakType::Bool => KoakTypeKind::UnsignedInteger,
            KoakType::Char | KoakType::Int => KoakTypeKind::Integer,
            KoakType::Double => KoakTypeKind::FloatingPoint,
            _ => KoakTypeKind::Other,
        }
    }
}

impl fmt::Display for KoakType {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            KoakType::Void => write!(f, "void"),
            KoakType::Bool => write!(f, "bool"),
            KoakType::Char => write!(f, "char"),
            KoakType::Int => write!(f, "int"),
            KoakType::Double => write!(f, "double"),
        }
    }
}

///
/// Try to cast `val` to `ty`.
/// Does nothing if `val` is already of type `ty`.
///
pub fn cast_to(token: &Token, val: KoakValue, ty: KoakType, context: &mut IRContext) -> IRExprResult {
    let val_ref = val.llvm_ref;
    let ty_ref = ty.as_llvm_ref();
    let new_val_ref = match (val.ty, ty) {
        (KoakType::Bool, KoakType::Bool)
            | (KoakType::Char, KoakType::Char)
            | (KoakType::Int, KoakType::Int)
            | (KoakType::Double, KoakType::Double)
                => val.llvm_ref,
        (KoakType::Bool, KoakType::Char) => context.builder.build_zext(val_ref, ty_ref, "cast_i1_i8"),
        (KoakType::Bool, KoakType::Int) => context.builder.build_zext(val_ref, ty_ref, "cast_i1_i32"),
        (KoakType::Bool, KoakType::Double) => context.builder.build_ui_to_fp(val_ref, ty_ref, "cast_i1_double"),
        (KoakType::Char, KoakType::Bool) => {
            let zero = IntConstRef::get(&IntTypeRef::get_int8(), 0, true).to_ref();
            context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, val_ref, zero, "cast_i8_i1")
        },
        (KoakType::Char, KoakType::Int) => context.builder.build_sext(val_ref, ty_ref, "cast_i8_i32"),
        (KoakType::Char, KoakType::Double) => context.builder.build_si_to_fp(val_ref, ty_ref, "cast_i8_double"),
        (KoakType::Int, KoakType::Bool) => {
            let zero = IntConstRef::get(&IntTypeRef::get_int32(), 0, true).to_ref();
            context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, val_ref, zero, "cast_i32_i1")
        },
        (KoakType::Int, KoakType::Char) => context.builder.build_trunc(val_ref, ty_ref, "cast_i32_i8"),
        (KoakType::Int, KoakType::Double) => context.builder.build_si_to_fp(val_ref, ty_ref, "cast_i32_double"),
        (KoakType::Double, KoakType::Bool) => {
            let zero = RealConstRef::get(&RealTypeRef::get_double(), 0.0).to_ref();
            context.builder.build_fcmp(LLVMRealPredicate::LLVMRealONE, val_ref, zero, "cast_double_i1")
        },
        (KoakType::Double, KoakType::Char) => context.builder.build_fp_to_si(val_ref, ty_ref, "cast_double_i8"),
        (KoakType::Double, KoakType::Int) => context.builder.build_fp_to_si(val_ref, ty_ref, "cast_double_i32"),
        _ => { return Err(SyntaxError::from(token, ErrorReason::CantCastTo(val.ty, ty))); } ,
    };
    Ok(KoakValue::new(new_val_ref, ty))
}

///
/// Tryes to calculate a common type to operate between two types.
/// This function must be permutative: it must return the same value even if `lhs` and `rhs` are reversed.
///
fn calculate_common(lhs: KoakValue, rhs: KoakValue) -> Option<KoakType> {
    match lhs.ty {
        KoakType::Bool => match rhs.ty {
            KoakType::Bool | KoakType::Char | KoakType::Int | KoakType::Double => Some(rhs.ty),
            _ => None,
        },
        KoakType::Char => match rhs.ty {
            KoakType::Bool | KoakType::Char => Some(lhs.ty),
            KoakType::Int | KoakType::Double => Some(rhs.ty),
            _ => None,
        },
        KoakType::Int => match rhs.ty {
            KoakType::Bool | KoakType::Char | KoakType::Int => Some(lhs.ty),
            KoakType::Double => Some(rhs.ty),
            _ => None,
        },
        KoakType::Double => match rhs.ty {
            KoakType::Bool | KoakType::Char | KoakType::Int | KoakType::Double => Some(KoakType::Double),
            _ => None,
        },
        _ => None,
    }
}

macro_rules! binop {
    ( $context:expr, $lhs:expr, $rhs:expr, $token:expr ) => {
        match calculate_common($lhs, $rhs) {
            Some(ty) => {
                let _lhs = cast_to($token, $lhs, ty, $context)?;
                let _rhs = cast_to($token, $rhs, ty, $context)?;
                Ok((ty, _lhs.llvm_ref, _rhs.llvm_ref))
            },
            None => Err(SyntaxError::from($token, ErrorReason::IncompatibleBinOp($lhs.ty, $rhs.ty)))
        }
    };
}

///
/// Finds
///

pub trait KoakCalculable {
    // Binary Operators
    fn add(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;
    fn sub(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;
    fn mul(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;
    fn div(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;
    fn rem(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;
    fn lt(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;
    fn gt(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;
    fn eq(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;
    fn le(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;
    fn ge(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;
    fn ne(&self, &mut IRContext, &Token, LLVMValueRef) -> IRExprResult;

    // Unary Operators
    fn unary_not(&self, &mut IRContext, &Token) -> IRExprResult;
}

impl KoakCalculable for KoakValue {
    // Binary Operators
    fn add(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, *self, rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer | KoakTypeKind::UnsignedInteger => Ok(context.builder.build_add(lhs_ref, rhs_ref, "addtmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fadd(lhs_ref, rhs_ref, "faddtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    fn sub(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, *self, rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer | KoakTypeKind::UnsignedInteger => Ok(context.builder.build_sub(lhs_ref, rhs_ref, "subtmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fsub(lhs_ref, rhs_ref, "fsubtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    fn mul(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, *self, rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer | KoakTypeKind::UnsignedInteger => Ok(context.builder.build_mul(lhs_ref, rhs_ref, "multmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fmul(lhs_ref, rhs_ref, "fmultmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    fn div(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, *self, rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_sdiv(lhs_ref, rhs_ref, "sdivtmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_udiv(lhs_ref, rhs_ref, "udivtmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fdiv(lhs_ref, rhs_ref, "fdivtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    fn rem(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, *self, rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_srem(lhs_ref, rhs_ref, "sremtmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_urem(lhs_ref, rhs_ref, "uremtmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_frem(lhs_ref, rhs_ref, "fremtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    fn lt(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, *self, rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntSLT, lhs_ref, rhs_ref, "icmptmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntULT, lhs_ref, rhs_ref, "ucmptmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOLT, lhs_ref, rhs_ref, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    fn gt(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, *self, rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntSGT, lhs_ref, rhs_ref, "icmptmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntUGT, lhs_ref, rhs_ref, "ucmptmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOGT, lhs_ref, rhs_ref, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    fn eq(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntEQ, lhs, rhs, "icmptmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOEQ, lhs, rhs, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
    }

    fn le(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntSLE, lhs, rhs, "icmptmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOLE, lhs, rhs, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
    }

    fn ge(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntSGE, lhs, rhs, "icmptmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOGE, lhs, rhs, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
    }

    fn ne(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, lhs, rhs, "icmptmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealONE, lhs, rhs, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
    }

    // Unary Operators
    fn unary_not(&self, context: &mut IRContext, token: &Token) -> IRExprResult {
        let new_val = match self.ty.get_kind() {
            KoakTypeKind::Integer | KoakTypeKind::UnsignedInteger => Ok(context.builder.build_neg(self.llvm_ref, "nottmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fneg(self.llvm_ref, "fnottmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleUnaryOp(self.ty))),
        }?;
        Ok(KoakValue::new(new_val, self.ty))
    }
}
