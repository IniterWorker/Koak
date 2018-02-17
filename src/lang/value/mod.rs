//!
//! Koak's type system
//!

use llvm_sys::prelude::LLVMValueRef;

mod double_val;
mod int_val;

pub use self::double_val::*;
pub use self::int_val::*;

use codegen::{IRContext, IRExprResult};
use lexer::Token;
use lang::Type;

///
/// A trait that all typed value must implement.
///
pub trait Value {
    fn get_type(&self) -> Type;
    fn as_llvm_ref(&self) -> LLVMValueRef;
    fn as_calculable(&self) -> &CalculableValue;
    fn clone_value(&self) -> Box<Value>;
}

pub trait CalculableValue {
    // Binary operators
    fn add(&self, &mut IRContext, &Token, &Value) -> IRExprResult;
    fn sub(&self, &mut IRContext, &Token, &Value) -> IRExprResult;
    fn mul(&self, &mut IRContext, &Token, &Value) -> IRExprResult;
    fn div(&self, &mut IRContext, &Token, &Value) -> IRExprResult;
    fn rem(&self, &mut IRContext, &Token, &Value) -> IRExprResult;
    fn less(&self, &mut IRContext, &Token, &Value) -> IRExprResult;
    fn more(&self, &mut IRContext, &Token, &Value) -> IRExprResult;

    // Unary Operators
    fn unary_plus(&self, &mut IRContext, &Token) -> IRExprResult;
    fn unary_minus(&self, &mut IRContext, &Token) -> IRExprResult;

    // TODO: Change this to a generic cast to bool
    fn cmp_zero(&self, &mut IRContext) -> IRExprResult;
}
