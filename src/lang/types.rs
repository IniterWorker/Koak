//!
//! Koak's type system.
//!
//!

use llvm_sys::LLVMTypeKind;
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};
use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};

use iron_llvm::{LLVMRef, LLVMRefCtor};
use iron_llvm::core::value::{Value, RealConstRef, IntConstRef, RealConstCtor, IntConstCtor};
use iron_llvm::core::types::{IntType, IntTypeRef, IntTypeCtor, RealTypeRef, RealTypeCtor, VoidTypeRef, VoidTypeCtor, Type};

use error::{ErrorReason, SyntaxError};
use codegen::{IRContext, IRResult};
use lexer::{Token, TokenType};

#[derive(Debug, Copy, Clone)]
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
        match self {
            &KoakType::Void => VoidTypeRef::get().to_ref(),
            &KoakType::Bool => IntTypeRef::get_int1().to_ref(),
            &KoakType::Char => IntTypeRef::get_int8().to_ref(),
            &KoakType::Int => IntTypeRef::get_int32().to_ref(),
            &KoakType::Double => RealTypeRef::get_double().to_ref(),
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
}

impl Into<KoakType> for LLVMTypeRef {
    fn into(self) -> KoakType {
        match self.get_kind() {
            LLVMTypeKind::LLVMVoidTypeKind => KoakType::Void,
            LLVMTypeKind::LLVMDoubleTypeKind => KoakType::Double,
            LLVMTypeKind::LLVMIntegerTypeKind => {
                let int = unsafe { IntTypeRef::from_ref(self) };
                match int.get_width() {
                    1 => KoakType::Bool,
                    8 => KoakType::Char,
                    32 => KoakType::Int,
                    _ => panic!("Unknown integer width"),
                }
            }
            LLVMTypeKind::LLVMFunctionTypeKind => panic!("Woops func"),
            _ => panic!("Unknown value type"),
        }
    }
}
///
/// Try to cast `val` to `ty`.
/// Does nothing if `val` is already of type `ty`.
///
pub fn cast_to(token: &Token, val: LLVMValueRef, ty: LLVMTypeRef, context: &mut IRContext) -> IRResult {
    match (val.get_type().into(), ty.into()) {
        (KoakType::Bool, KoakType::Bool) => Ok(val),
        (KoakType::Bool, KoakType::Char) => Ok(context.builder.build_zext(val, ty, "cast_i1_i8")),
        (KoakType::Bool, KoakType::Int) => Ok(context.builder.build_zext(val, ty, "cast_i1_i32")),
        (KoakType::Bool, KoakType::Double) => Ok(context.builder.build_ui_to_fp(val, ty, "cast_i1_double")),
        (KoakType::Char, KoakType::Bool) => {
            let zero = IntConstRef::get(&IntTypeRef::get_int8(), 0, true).to_ref();
            Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, val, zero, "cast_i8_i1"))
        },
        (KoakType::Char, KoakType::Char) => Ok(val),
        (KoakType::Char, KoakType::Int) => Ok(context.builder.build_zext(val, ty, "cast_i8_i32")),
        (KoakType::Char, KoakType::Double) => Ok(context.builder.build_si_to_fp(val, ty, "cast_i8_double")),
        (KoakType::Int, KoakType::Bool) => {
            let zero = IntConstRef::get(&IntTypeRef::get_int32(), 0, true).to_ref();
            Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, val, zero, "cast_i32_i1"))
        },
        (KoakType::Int, KoakType::Char) => Ok(context.builder.build_trunc(val, ty, "cast_i32_i8")),
        (KoakType::Int, KoakType::Int) => Ok(val),
        (KoakType::Int, KoakType::Double) => Ok(context.builder.build_si_to_fp(val, ty, "cast_i32_double")),
        (KoakType::Double, KoakType::Bool) => {
            let zero = RealConstRef::get(&RealTypeRef::get_double(), 0.0).to_ref();
            Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealONE, val, zero, "cast_double_i1"))
        },
        (KoakType::Double, KoakType::Char) => Ok(context.builder.build_fp_to_si(val, ty, "cast_double_i8")),
        (KoakType::Double, KoakType::Int) => Ok(context.builder.build_fp_to_si(val, ty, "cast_double_i32")),
        (KoakType::Double, KoakType::Double) => Ok(val),
        _ => Err(SyntaxError::from(token, ErrorReason::CantCastTo(val.get_type(), ty))),
    }
}

///
/// Tryes to calculate a common type to operate between two types.
///
fn calculate_common(lhs: LLVMValueRef, rhs: LLVMValueRef) -> Option<LLVMTypeRef> {
    match (lhs.get_type().get_kind(), rhs.get_type().get_kind()) {
        (LLVMTypeKind::LLVMIntegerTypeKind, LLVMTypeKind::LLVMIntegerTypeKind) => {
            let lhs_ty = unsafe { IntTypeRef::from_ref(lhs.get_type()) };
            let rhs_ty = unsafe { IntTypeRef::from_ref(rhs.get_type()) };
            if lhs_ty.get_width() <= rhs_ty.get_width() { // Chose the smaller integer type
                Some(rhs.get_type())
            } else {
                Some(lhs.get_type())
            }
        }
        (LLVMTypeKind::LLVMDoubleTypeKind, LLVMTypeKind::LLVMIntegerTypeKind) => Some(lhs.get_type()),
        (LLVMTypeKind::LLVMIntegerTypeKind, LLVMTypeKind::LLVMDoubleTypeKind) => Some(rhs.get_type()),
        (LLVMTypeKind::LLVMDoubleTypeKind, LLVMTypeKind::LLVMDoubleTypeKind) => Some(lhs.get_type()),
        _ => None,
    }
}

macro_rules! binop {
    ( $context:expr, $lhs:expr, $rhs:expr, $token:expr ) => {
        match calculate_common($lhs, $rhs) {
            Some(ty) => {
                let _lhs = cast_to($token, $lhs, ty, $context)?;
                let _rhs = cast_to($token, $rhs, ty, $context)?;
                Ok((ty.get_kind(), _lhs, _rhs))
            },
            None => Err(SyntaxError::from($token, ErrorReason::IncompatibleBinOp($lhs.get_type(), $rhs.get_type())))
        }
    };
}

///
/// Finds
///

pub trait KoakCalculable {
    // Binary Operators
    fn add(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;
    fn sub(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;
    fn mul(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;
    fn div(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;
    fn rem(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;
    fn lt(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;
    fn gt(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;
    fn eq(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;
    fn le(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;
    fn ge(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;
    fn ne(&self, &mut IRContext, &Token, LLVMValueRef) -> IRResult;

    // Unary Operators
    fn unary_not(&self, &mut IRContext, &Token) -> IRResult;
}

impl KoakCalculable for LLVMValueRef {
    // Binary Operators
    fn add(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_add(lhs, rhs, "addtmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_fadd(lhs, rhs, "faddtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
    }

    fn sub(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_sub(lhs, rhs, "subtmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_fsub(lhs, rhs, "fsubtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
    }

    fn mul(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_mul(lhs, rhs, "multmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_fmul(lhs, rhs, "fmultmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
    }

    fn div(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_sdiv(lhs, rhs, "sdivtmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_fdiv(lhs, rhs, "fdivtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
    }

    fn rem(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_srem(lhs, rhs, "sremtmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_frem(lhs, rhs, "fremtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
    }

    fn lt(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntSLT, lhs, rhs, "icmptmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOLT, lhs, rhs, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
    }

    fn gt(&self, context: &mut IRContext, token: &Token, rhs: LLVMValueRef) -> IRResult {
        match binop!(context, *self, rhs, token)? {
            (LLVMTypeKind::LLVMIntegerTypeKind, lhs, rhs) => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntSGT, lhs, rhs, "icmptmp")),
            (LLVMTypeKind::LLVMDoubleTypeKind, lhs, rhs) => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOGT, lhs, rhs, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.get_type(), rhs.get_type())))
        }
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
    fn unary_not(&self, context: &mut IRContext, token: &Token) -> IRResult {
        match self.get_type().get_kind() {
            LLVMTypeKind::LLVMIntegerTypeKind => Ok(context.builder.build_neg(*self, "nottmp")),
            LLVMTypeKind::LLVMDoubleTypeKind => Ok(context.builder.build_fneg(*self, "fnottmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleUnaryOp(self.get_type()))),
        }
    }
}
