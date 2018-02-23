//!
//! Koak's values
//!

use std::ptr;

use lang::types::KoakType;

use llvm_sys::prelude::LLVMValueRef;

#[derive(Copy, Clone)]
pub struct KoakValue {
    pub llvm_ref: LLVMValueRef,
    pub ty: KoakType,
}

impl KoakValue {
    #[inline]
    pub fn new(val: LLVMValueRef, ty: KoakType) -> KoakValue {
        KoakValue {
            llvm_ref: val,
            ty: ty,
        }
    }

    #[inline]
    pub fn new_void() -> KoakValue {
        KoakValue {
            llvm_ref: ptr::null_mut(),
            ty: KoakType::Void,
        }
    }
}
