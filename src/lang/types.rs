//!
//! Koak's type system.
//!
//!

use std::fmt;

use llvm_sys::prelude::{LLVMTypeRef};

use iron_llvm::{LLVMRef};
use iron_llvm::core::types::{IntTypeRef, IntTypeCtor, RealTypeRef, RealTypeCtor, VoidTypeRef, VoidTypeCtor};

use error::{ErrorReason, SyntaxError};
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
/// Tryes to calculate a common type to operate between two types.
/// This function must be permutative: it must return the same value even if `lhs` and `rhs` are reversed.
///
pub fn calculate_common(lhs: &KoakValue, rhs: &KoakValue) -> Option<KoakType> {
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
