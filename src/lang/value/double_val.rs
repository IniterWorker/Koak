//!
//! Double - A 64-bits floating number.
//!

use llvm_sys::LLVMOpcode;
use llvm_sys::LLVMRealPredicate;
use llvm_sys::LLVMRealPredicate::{LLVMRealOLT, LLVMRealOGT};
use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::LLVMRef;
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef};
use iron_llvm::core::value::{RealConstRef, RealConstCtor};

use lang::Type;
use lexer::{Token, OperatorType};
use error::{SyntaxError, ErrorReason};
use lang::value::{Value, CalculableValue};
use codegen::{IRContext, IRExprResult};

#[derive(Clone)]
pub struct DoubleValue {
    llvm_ref: LLVMValueRef,
}

impl DoubleValue {
    pub fn new(llvm_ref: LLVMValueRef) -> Box<DoubleValue> {
        Box::new(DoubleValue {
            llvm_ref: llvm_ref,
        })
    }
}

impl Value for DoubleValue {
    fn get_type(&self) -> Type {
        Type::Double
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

impl CalculableValue for DoubleValue {

    #[allow(unreachable_patterns)]
    fn add(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Double => Ok(Type::Double.new_value(context.builder.build_fadd(self.llvm_ref, rhs.as_llvm_ref(), "addtmp"))?),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Add, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn sub(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Double => Ok(Type::Double.new_value(context.builder.build_fsub(self.llvm_ref, rhs.as_llvm_ref(), "subtmp"))?),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Sub, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn mul(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Double => Ok(Type::Double.new_value(context.builder.build_fmul(self.llvm_ref, rhs.as_llvm_ref(), "multmp"))?),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Mul, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn div(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Double => Ok(Type::Double.new_value(context.builder.build_fdiv(self.llvm_ref, rhs.as_llvm_ref(), "divtmp"))?),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Div, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn rem(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Double => Ok(Type::Double.new_value(context.builder.build_frem(self.llvm_ref, rhs.as_llvm_ref(), "remtmp"))?),
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Rem, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn less(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Double => {
                let tmp = context.builder.build_fcmp(LLVMRealOLT, self.llvm_ref, rhs.as_llvm_ref(), "cmptmp");
                Ok(Type::Double.new_value(context.builder.build_cast(LLVMOpcode::LLVMUIToFP, tmp, self.get_type().as_llvm_ref(), "casttmp"))?)
            },
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Rem, self.get_type(), rhs.get_type()))),
        }
    }

    #[allow(unreachable_patterns)]
    fn more(&self, context: &mut IRContext, token: &Token, rhs: &Value) -> IRExprResult {
        match rhs.get_type() {
            Type::Double => {
                let tmp = context.builder.build_fcmp(LLVMRealOGT, self.llvm_ref, rhs.as_llvm_ref(), "cmptmp");
                Ok(Type::Double.new_value(context.builder.build_cast(LLVMOpcode::LLVMUIToFP, tmp, self.get_type().as_llvm_ref(), "casttmp"))?)
            },
            _ => Err(SyntaxError::from(token, ErrorReason::IncompatibleBinOp(OperatorType::Rem, self.get_type(), rhs.get_type()))),
        }
    }

    fn unary_plus(&self, _: &mut IRContext, _: &Token) -> IRExprResult {
        Ok(Type::Double.new_value(self.llvm_ref)?)
    }

    fn unary_minus(&self, context: &mut IRContext, _: &Token) -> IRExprResult {
        Ok(Type::Double.new_value(context.builder.build_fneg(self.llvm_ref, "unaryneg"))?)
    }

    fn cmp_zero(&self, context: &mut IRContext) -> IRExprResult {
        let zero = RealConstRef::get(&RealTypeRef::get_double(), 0.0).to_ref(); // TODO Improve Litterals
        Ok(Type::Double.new_value(context.builder.build_fcmp(LLVMRealPredicate::LLVMRealONE, self.llvm_ref, zero.to_ref(), "cond"))?)
    }
}
