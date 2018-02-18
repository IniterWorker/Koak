//!
//! Koak's type system.
//!
//!

use llvm_sys::LLVMTypeKind;
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};
use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};

use iron_llvm::LLVMRefCtor;
use iron_llvm::core::value::Value;
use iron_llvm::core::types::{IntType, IntTypeRef, Type};

use error::{ErrorReason, SyntaxError};
use codegen::{IRContext, IRResult};
use lexer::Token;

///
/// Try to cast `val` to `ty`.
/// Does nothing if `val` is already of type `ty`.
///
pub fn cast_to(token: &Token, val: LLVMValueRef, ty: LLVMTypeRef, context: &mut IRContext) -> IRResult {
    match (val.get_type().get_kind(), ty.get_kind()) {
        (LLVMTypeKind::LLVMDoubleTypeKind, LLVMTypeKind::LLVMDoubleTypeKind) => Ok(val),
        (LLVMTypeKind::LLVMIntegerTypeKind, LLVMTypeKind::LLVMIntegerTypeKind) => {
            let lhs_ty = unsafe { IntTypeRef::from_ref(val.get_type()) };
            let rhs_ty = unsafe { IntTypeRef::from_ref(ty) };
            if lhs_ty.get_width() != rhs_ty.get_width() {
                // Cast only if integer type is different
                Ok(context.builder.build_int_cast(val, ty, "cast_si_si"))
            } else {
                Ok(val)
            }
        }
        (LLVMTypeKind::LLVMDoubleTypeKind, LLVMTypeKind::LLVMIntegerTypeKind) => {
            Ok(context.builder.build_fp_to_si(val, ty, "cast_fp_si"))
        }
        (LLVMTypeKind::LLVMIntegerTypeKind, LLVMTypeKind::LLVMDoubleTypeKind) => {
            Ok(context.builder.build_si_to_fp(val, ty, "cast_si_fp"))
        }
        _ => Err(SyntaxError::from(
            &token,
            ErrorReason::CantCastTo(val.get_type(), ty),
        )),
    }
}

///
/// Tryes to calculate a common type to operate between two types.
///
pub fn calculate_common(lhs: LLVMValueRef, rhs: LLVMValueRef) -> Option<LLVMTypeRef> {
    match (lhs.get_type().get_kind(), rhs.get_type().get_kind()) {
        (LLVMTypeKind::LLVMIntegerTypeKind, LLVMTypeKind::LLVMIntegerTypeKind) => {
            let lhs_ty = unsafe { IntTypeRef::from_ref(lhs.get_type()) };
            let rhs_ty = unsafe { IntTypeRef::from_ref(rhs.get_type()) };
            if lhs_ty.get_width() <= rhs_ty.get_width() {
                // Chose the smaller integer type
                Some(lhs.get_type())
            } else {
                Some(rhs.get_type())
            }
        }
        (LLVMTypeKind::LLVMDoubleTypeKind, LLVMTypeKind::LLVMIntegerTypeKind) => Some(lhs.get_type()),
        (LLVMTypeKind::LLVMIntegerTypeKind, LLVMTypeKind::LLVMDoubleTypeKind) => Some(rhs.get_type()),
        _ => Some(lhs.get_type()),
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

    // Unary Operators
    fn unary_not(&self, context: &mut IRContext, token: &Token) -> IRResult {
        match self.get_type().get_kind() {
            LLVMTypeKind::LLVMIntegerTypeKind => Ok(context.builder.build_neg(*self, "nottmp")),
            LLVMTypeKind::LLVMDoubleTypeKind => Ok(context.builder.build_fneg(*self, "fnottmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleUnaryOp(self.get_type()))),
        }
    }
}
