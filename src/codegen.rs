//!
//! Koak's code generation
//!

use std::collections::HashMap;

use iron_llvm::core;
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef};
use llvm_sys::prelude::LLVMValueRef;

pub struct CodeGen {
    context: core::Context,
    builder: core::Builder,
    named_values: HashMap<String, LLVMValueRef>,
    double_type: RealTypeRef,
}

impl CodeGen {
    pub fn new() -> CodeGen {
        CodeGen {
            context: core::Context::get_global(),
            builder: core::Builder::new(),
            named_values: HashMap::new(),
            double_type: RealTypeRef::get_double(),
        }
    }
}
