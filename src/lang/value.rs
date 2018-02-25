//!
//! Koak's values
//!

use std::ptr;
use std::rc::Rc;

use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};

use iron_llvm::LLVMRef;
use iron_llvm::core::value::{RealConstRef, IntConstRef, RealConstCtor, IntConstCtor};
use iron_llvm::core::types::{IntTypeRef, IntTypeCtor, RealTypeRef, RealTypeCtor};

use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRExprResult};
use lexer::Token;
use lang::types::{KoakType, KoakTypeKind, calculate_common};
use llvm_sys::LLVMOpcode;
use llvm_sys::prelude::LLVMValueRef;

#[derive(Debug, Clone)]
pub struct KoakValue {
    pub llvm_ref: LLVMValueRef,
    pub ty: KoakType,
    pub is_mut: bool,
    pub name: Option<Rc<String>>,
}

macro_rules! binop {
    ( $context:expr, $lhs:expr, $rhs:expr, $token:expr ) => {
        match calculate_common($lhs, $rhs) {
            Some(ty) => {
                let _lhs = $lhs.cast_to($token, $context, ty)?;
                let _rhs = $rhs.cast_to($token, $context, ty)?;
                Ok((ty, _lhs.llvm_ref, _rhs.llvm_ref))
            },
            None => Err(SyntaxError::from($token, ErrorReason::IncompatibleBinOp($lhs.ty, $rhs.ty)))
        }
    };
}

macro_rules! assignop {
    ( $context:expr, $lhs:expr, $rhs:expr, $token:expr ) => {
        if $lhs.name.is_some() {
            let name = $lhs.name.clone().unwrap();
            if $lhs.is_mut {
                let rhs = $rhs.cast_to($token, $context, $lhs.ty)?;
                Ok((name, rhs))
            } else {
                Err(SyntaxError::from($token, ErrorReason::ReassigningConstVar((&name as &str).to_string())))
            }
        } else {
            Err(SyntaxError::from($token, ErrorReason::AssigningRvalue))
        }
    }
}


impl KoakValue {
    #[inline]
    pub fn new(val: LLVMValueRef, ty: KoakType) -> KoakValue {
        KoakValue {
            llvm_ref: val,
            ty: ty,
            is_mut: false,
            name: None,
        }
    }

    #[inline]
    pub fn new_var(name: Rc<String>, val: LLVMValueRef, ty: KoakType) -> KoakValue {
        KoakValue {
            llvm_ref: val,
            ty: ty,
            is_mut: false,
            name: Some(name),
        }
    }

    #[inline]
    pub fn new_void() -> KoakValue {
        KoakValue {
            llvm_ref: ptr::null_mut(),
            ty: KoakType::Void,
            is_mut: false,
            name: None,
        }
    }

    #[inline]
    pub fn copy_and_reassign(&self, val: LLVMValueRef) -> KoakValue {
        KoakValue {
            llvm_ref: val,
            ty: self.ty,
            is_mut: self.is_mut,
            name: self.name.clone(),
        }
    }

    // Binary operators

    pub fn add(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer | KoakTypeKind::UnsignedInteger => Ok(context.builder.build_add(lhs_ref, rhs_ref, "addtmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fadd(lhs_ref, rhs_ref, "faddtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    pub fn sub(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer | KoakTypeKind::UnsignedInteger => Ok(context.builder.build_sub(lhs_ref, rhs_ref, "subtmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fsub(lhs_ref, rhs_ref, "fsubtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    pub fn mul(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer | KoakTypeKind::UnsignedInteger => Ok(context.builder.build_mul(lhs_ref, rhs_ref, "multmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fmul(lhs_ref, rhs_ref, "fmultmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    pub fn div(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_sdiv(lhs_ref, rhs_ref, "sdivtmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_udiv(lhs_ref, rhs_ref, "udivtmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fdiv(lhs_ref, rhs_ref, "fdivtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    pub fn rem(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_srem(lhs_ref, rhs_ref, "sremtmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_urem(lhs_ref, rhs_ref, "uremtmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_frem(lhs_ref, rhs_ref, "fremtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    pub fn lt(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntSLT, lhs_ref, rhs_ref, "icmptmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntULT, lhs_ref, rhs_ref, "ucmptmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOLT, lhs_ref, rhs_ref, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    pub fn gt(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntSGT, lhs_ref, rhs_ref, "icmptmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntUGT, lhs_ref, rhs_ref, "ucmptmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOGT, lhs_ref, rhs_ref, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    // Assign operators
    pub fn assign(&mut self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> Result<(), SyntaxError> {
        let (name, rhs) = assignop!(context, self, rhs, token)?;
        self.llvm_ref = rhs.llvm_ref;
        context.update_local_var(token, name.clone(), self.llvm_ref)?;
        Ok(())
    }

    pub fn add_assign(&mut self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> Result<(), SyntaxError> {
        let (name, rhs) = assignop!(context, self, rhs, token)?;
        let tmp = self.add(context, token, rhs)?;
        self.llvm_ref = tmp.llvm_ref;
        context.update_local_var(token, name.clone(), self.llvm_ref)?;
        Ok(())
    }

    pub fn sub_assign(&mut self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> Result<(), SyntaxError> {
        let (name, rhs) = assignop!(context, self, rhs, token)?;
        let tmp = self.sub(context, token, rhs)?;
        self.llvm_ref = tmp.llvm_ref;
        context.update_local_var(token, name.clone(), self.llvm_ref)?;
        Ok(())
    }

    pub fn mul_assign(&mut self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> Result<(), SyntaxError> {
        let (name, rhs) = assignop!(context, self, rhs, token)?;
        let tmp = self.mul(context, token, rhs)?;
        self.llvm_ref = tmp.llvm_ref;
        context.update_local_var(token, name.clone(), self.llvm_ref)?;
        Ok(())
    }

    pub fn div_assign(&mut self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> Result<(), SyntaxError> {
        let (name, rhs) = assignop!(context, self, rhs, token)?;
        let tmp = self.div(context, token, rhs)?;
        self.llvm_ref = tmp.llvm_ref;
        context.update_local_var(token, name.clone(), self.llvm_ref)?;
        Ok(())
    }

    pub fn rem_assign(&mut self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> Result<(), SyntaxError> {
        let (name, rhs) = assignop!(context, self, rhs, token)?;
        let tmp = self.rem(context, token, rhs)?;
        self.llvm_ref = tmp.llvm_ref;
        context.update_local_var(token, name.clone(), self.llvm_ref)?;
        Ok(())
    }

    pub fn eq(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntEQ, lhs_ref, rhs_ref, "icmptmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntEQ, lhs_ref, rhs_ref, "ucmptmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOEQ, lhs_ref, rhs_ref, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    pub fn le(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntSLE, lhs_ref, rhs_ref, "icmptmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntULE, lhs_ref, rhs_ref, "ucmptmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOEQ, lhs_ref, rhs_ref, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    pub fn ge(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntSGE, lhs_ref, rhs_ref, "icmptmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntUGE, lhs_ref, rhs_ref, "ucmptmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealOGE, lhs_ref, rhs_ref, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    pub fn ne(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, lhs_ref, rhs_ref, "icmptmp")),
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, lhs_ref, rhs_ref, "ucmptmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealONE, lhs_ref, rhs_ref, "fcmptmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    pub fn shl(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer |
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_binop(LLVMOpcode::LLVMShl, lhs_ref, rhs_ref, "shltmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    pub fn shr(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer |
            KoakTypeKind::UnsignedInteger => {
                context.builder.build_zext(self.llvm_ref, self.ty.as_llvm_ref(), "cast_i1_i32");
                context.builder.build_zext(rhs.llvm_ref, rhs.ty.as_llvm_ref(), "cast_i1_i32");
                Ok(context.builder.build_binop(LLVMOpcode::LLVMAShr, lhs_ref, rhs_ref, "shrtmp"))
            },
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    pub fn and(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, _lhs_ref, _rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    pub fn or(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, _lhs_ref, _rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    pub fn bitwise_or(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer |
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_or(lhs_ref, rhs_ref, "bortmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    pub fn bitwise_and(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer |
            KoakTypeKind::UnsignedInteger =>
                Ok(context.builder.build_and(lhs_ref, rhs_ref, "bandtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    pub fn bitwise_xor(&self, context: &mut IRContext, token: &Token, rhs: KoakValue) -> IRExprResult {
        let (new_ty, lhs_ref, rhs_ref) = binop!(context, self, &rhs, token)?;
        let new_val = match new_ty.get_kind() {
            KoakTypeKind::Integer |
            KoakTypeKind::UnsignedInteger =>
                Ok(context.builder.build_xor(lhs_ref, rhs_ref, "bxortmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(self.ty, rhs.ty)))
        }?;
        Ok(KoakValue::new(new_val, new_ty))
    }

    // Unary Operators
    pub fn unary_not(&self, context: &mut IRContext, token: &Token) -> IRExprResult {
        let new_val = match self.ty.get_kind() {
            KoakTypeKind::Integer |
            KoakTypeKind::UnsignedInteger |
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_is_null(self.llvm_ref, "nottmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleUnaryOp(self.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Bool))
    }

    pub fn unary_compl(&self, context: &mut IRContext, token: &Token) -> IRExprResult {
        let new_val = match self.ty.get_kind() {
            KoakTypeKind::Integer |
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_not(self.llvm_ref, "negtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleUnaryOp(self.ty)))
        }?;
        Ok(KoakValue::new(new_val, KoakType::Int))
    }

    pub fn unary_neg(&self, context: &mut IRContext, token: &Token) -> IRExprResult {
        let new_val = match self.ty.get_kind() {
            KoakTypeKind::Integer |
            KoakTypeKind::UnsignedInteger => Ok(context.builder.build_neg(self.llvm_ref, "negtmp")),
            KoakTypeKind::FloatingPoint => Ok(context.builder.build_fneg(self.llvm_ref, "negtmp")),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleUnaryOp(self.ty)))
        }?;
        Ok(KoakValue::new(new_val, self.ty))
    }

    ///
    /// Try to cast `val` to `ty`.
    /// Does nothing if `val` is already of type `ty`.
    ///
    pub fn cast_to(&self, token: &Token, context: &mut IRContext, ty: KoakType) -> Result<KoakValue, SyntaxError> {
        let ty_ref = ty.as_llvm_ref();
        let new_val_ref = match (self.ty, ty) {
            (KoakType::Bool, KoakType::Bool)
                | (KoakType::Char, KoakType::Char)
                | (KoakType::Int, KoakType::Int)
                | (KoakType::Double, KoakType::Double)
                    => self.llvm_ref,
            (KoakType::Bool, KoakType::Char) => context.builder.build_zext(self.llvm_ref, ty_ref, "cast_i1_i8"),
            (KoakType::Bool, KoakType::Int) => context.builder.build_zext(self.llvm_ref, ty_ref, "cast_i1_i32"),
            (KoakType::Bool, KoakType::Double) => context.builder.build_ui_to_fp(self.llvm_ref, ty_ref, "cast_i1_double"),
            (KoakType::Char, KoakType::Bool) => {
                let zero = IntConstRef::get(&IntTypeRef::get_int8(), 0, true).to_ref();
                context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, self.llvm_ref, zero, "cast_i8_i1")
            },
            (KoakType::Char, KoakType::Int) => context.builder.build_sext(self.llvm_ref, ty_ref, "cast_i8_i32"),
            (KoakType::Char, KoakType::Double) => context.builder.build_si_to_fp(self.llvm_ref, ty_ref, "cast_i8_double"),
            (KoakType::Int, KoakType::Bool) => {
                let zero = IntConstRef::get(&IntTypeRef::get_int32(), 0, true).to_ref();
                context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, self.llvm_ref, zero, "cast_i32_i1")
            },
            (KoakType::Int, KoakType::Char) => context.builder.build_trunc(self.llvm_ref, ty_ref, "cast_i32_i8"),
            (KoakType::Int, KoakType::Double) => context.builder.build_si_to_fp(self.llvm_ref, ty_ref, "cast_i32_double"),
            (KoakType::Double, KoakType::Bool) => {
                let zero = RealConstRef::get(&RealTypeRef::get_double(), 0.0).to_ref();
                context.builder.build_fcmp(LLVMRealPredicate::LLVMRealONE, self.llvm_ref, zero, "cast_double_i1")
            },
            (KoakType::Double, KoakType::Char) => context.builder.build_fp_to_si(self.llvm_ref, ty_ref, "cast_double_i8"),
            (KoakType::Double, KoakType::Int) => context.builder.build_fp_to_si(self.llvm_ref, ty_ref, "cast_double_i32"),
            _ => { return Err(SyntaxError::from(token, ErrorReason::CantCastTo(self.ty, ty))); } ,
        };
        // Update ref
        Ok(KoakValue::new(new_val_ref, ty))
    }
}
