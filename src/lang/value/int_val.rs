//!
//! Int - A 32-bits integer.
//!

use llvm_sys::LLVMOpcode;
use llvm_sys::LLVMIntPredicate;
use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::LLVMRef;
use iron_llvm::core::types::{IntTypeCtor, IntTypeRef};
use iron_llvm::core::value::{IntConstRef, IntConstCtor};

use lang::Type;
use lexer::{Token, OperatorType};
use error::{SyntaxError, ErrorReason};
use lang::value::{Value, CalculableValue};
use codegen::{IRContext, IRExprResult};

#[derive(Clone)]
pub struct IntValue {
    llvm_ref: LLVMValueRef,
}

impl IntValue {
    pub fn new(llvm_ref: LLVMValueRef) -> Box<IntValue> {
        Box::new(IntValue {
            llvm_ref: llvm_ref,
        })
    }
}

impl Into<Box<Value>> for i32 {
    fn into(self) -> Box<Value> {
        IntValue::new(IntConstRef::get(&IntTypeRef::get_int32(), self as u64, true).to_ref())
    }
}

impl Value for IntValue {
    fn get_type(&self) -> Type {
        Type::Int
    }

    fn as_llvm_ref(&self) -> LLVMValueRef {
        self.llvm_ref
    }

    fn as_calculable(&self) -> &CalculableValue {
        self
    }

    fn clone_value(&self) -> Box<Value> {
        Box::new(self.clone())
    }
}

impl CalculableValue for IntValue {

    #[allow(unreachable_patterns)]
    fn add(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Int => Ok(Type::Int.new_value(context.builder.build_add(self.llvm_ref, rhs.as_llvm_ref(), "addtmp"))?),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Add, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn sub(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Int=> Ok(Type::Int.new_value(context.builder.build_sub(self.llvm_ref, rhs.as_llvm_ref(), "subtmp"))?),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Sub, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn mul(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Int => Ok(Type::Int.new_value(context.builder.build_fmul(self.llvm_ref, rhs.as_llvm_ref(), "multmp"))?),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Mul, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn div(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Int => Ok(Type::Int.new_value(context.builder.build_sdiv(self.llvm_ref, rhs.as_llvm_ref(), "divtmp"))?),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Div, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn rem(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Int => Ok(Type::Int.new_value(context.builder.build_srem(self.llvm_ref, rhs.as_llvm_ref(), "remtmp"))?),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Rem, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn less(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Int => {
                let tmp = context.builder.build_icmp(LLVMIntPredicate::LLVMIntSLT, self.llvm_ref, rhs.as_llvm_ref(), "cmptmp");
                Ok(Type::Int.new_value(context.builder.build_cast(LLVMOpcode::LLVMUIToFP, tmp, self.get_type().as_llvm_ref(), "casttmp"))?)
            },
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Rem, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn more(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Int => {
                let tmp = context.builder.build_icmp(LLVMIntPredicate::LLVMIntSGT, self.llvm_ref, rhs.as_llvm_ref(), "cmptmp");
                Ok(Type::Int.new_value(context.builder.build_cast(LLVMOpcode::LLVMUIToFP, tmp, self.get_type().as_llvm_ref(), "casttmp"))?)
            },
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Rem, self.get_type(), rhs.get_type()))),
        }
    }

    fn unary_plus(&self, _: &mut IRContext, _: &Token) -> IRExprResult {
        Ok(Type::Int.new_value(self.llvm_ref)?)
    }

    fn unary_minus(&self, context: &mut IRContext, _: &Token) -> IRExprResult {
        Ok(Type::Int.new_value(context.builder.build_neg(self.llvm_ref, "unaryneg"))?)
    }

    fn cmp_zero(&self, context: &mut IRContext) -> IRExprResult {
        let zero: Box<Value> = 0.into();
        Ok(Type::Int.new_value(context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, self.llvm_ref, zero.as_llvm_ref(), "cond"))?)
    }
}
