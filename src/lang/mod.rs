//!
//! Koak's lang module
//!
//! All submodules implement a feature of the language. They both implement the parsing and the IR generation of it.
//!

use std::fmt;

use llvm_sys::prelude::*;
use llvm_sys::prelude::LLVMTypeRef;

use iron_llvm::core::types::{RealTypeCtor, RealTypeRef, IntTypeCtor, IntTypeRef};
use iron_llvm::{LLVMRef};

pub mod expr;
pub mod function;
pub mod cond;
pub mod variable;
pub mod value;

use self::value::{Value, DoubleValue, IntValue};
use error::SyntaxError;

///
/// An enum of all types
///
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Type {
    Double,
    Int,
}

impl Type {
    pub fn as_llvm_ref(&self) -> LLVMTypeRef {
        match self {
            &Type::Double => RealTypeRef::get_double().to_ref(),
            &Type::Int => IntTypeRef::get_int32().to_ref(),
        }
    }

    pub fn new_value(&self, llvm_ref: LLVMValueRef) -> Result<Box<Value>, SyntaxError> {
        match self {
            &Type::Double => Ok(DoubleValue::new(llvm_ref)),
            &Type::Int => Ok(IntValue::new(llvm_ref)),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Type::Double => write!(f, "double"),
            &Type::Int => write!(f, "int"),
        }
    }
}
