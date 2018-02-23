//!
//! Koak's anonymous functions (for top level expressions)
//!

use std::rc::Rc;

use llvm_sys::core::LLVMDeleteFunction;

use iron_llvm::LLVMRef;
use iron_llvm::core::value::{Function, FunctionRef, FunctionCtor};
use iron_llvm::core::types::{FunctionTypeCtor, FunctionTypeRef};

use lang::block::Block;
use lang::types::KoakType;
use lang::types;
use lang::function::FunctionPrototype;
use codegen::{IRContext, IRModuleProvider, IRExprGenerator, IRFuncGenerator, IRFuncResult};

pub struct AnonymousFunction {
    pub name: Rc<String>,
    pub body: Block,
}

impl AnonymousFunction {
    pub fn new(body: Block) -> AnonymousFunction {
        AnonymousFunction {
            body: body,
            name: Rc::new(AnonymousFunction::new_anonymous_name()),
        }
    }

    #[inline]
    pub fn new_anonymous_name() -> String {
        static mut NB_ANON: u64 = 0;

        let mut name = String::from("__"); // Create unique name for anonymous function
        unsafe {
            name += &NB_ANON.to_string();
            NB_ANON += 1;
        }
        name += "$"; // Prevent the name from being taken by a user-defined function
        name
    }
}

impl IRFuncGenerator for AnonymousFunction {
    #[inline]
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRFuncResult {
        // Create a temporary function to contain the body
        let func_ty_ref = FunctionTypeRef::get(&KoakType::Void.as_llvm_ref(), Vec::new().as_mut_slice(), false);
        let mut func = FunctionRef::new(&mut module_provider.get_module(), &self.name, &func_ty_ref);

        // Generate basic block that contain body
        let mut bb = func.append_basic_block_in_context(&mut context.context, "entry");
        context.builder.position_at_end(&mut bb);
        let body_r = self.body.gen_ir(context, module_provider);

        // Remove tmp func
        unsafe { LLVMDeleteFunction(func.to_ref()); }

        // Keep type or throw errors
        let body_ty = body_r?.ty;

        // Create definitive function
        let func_ty_ref = FunctionTypeRef::get(&body_ty.as_llvm_ref(), Vec::new().as_mut_slice(), false);
        let mut func = FunctionRef::new(&mut module_provider.get_module(), &self.name, &func_ty_ref);

        // Link it with the previously created one
        let mut bb = func.append_basic_block_in_context(&mut context.context, "entry");
        context.builder.position_at_end(&mut bb);

        do catch {
            // Generate body
            let ret_val = self.body.gen_ir(context, module_provider)?;

            // Append return
            if let KoakType::Void = body_ty {
                context.builder.build_ret_void();
            } else {
                // Cast it to return type
                let ret_casted = types::cast_to(&self.body.token, ret_val, body_ty, context)?;
                context.builder.build_ret(&ret_casted.llvm_ref);
            }
            context.functions.insert(self.name.clone(), Rc::new(FunctionPrototype::new(self.body.token.clone(), self.name.clone(), Vec::new(), body_ty)));
            Ok(func.to_ref())
        }.map_err(|e| unsafe {
            LLVMDeleteFunction(func.to_ref());
            e
        })
    }
}
