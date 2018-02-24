//!
//! This module implements control-flow
//!

use std::fmt;

use llvm_sys::LLVMIntPredicate;

use iron_llvm::{LLVMRef, LLVMRefCtor};
use iron_llvm::core::types::{IntTypeRef, IntTypeCtor};
use iron_llvm::core::instruction::{PHINode, PHINodeRef};
use iron_llvm::core::value::{Function, IntConstRef, IntConstCtor};
use iron_llvm::core::basic_block::*;

use lexer::TokenType;
use parser::Parser;
use lang::expr::{Expr, parse_expr};
use lang::types;
use lang::types::KoakType;
use lang::value::KoakValue;
use lang::block::{Block, parse_block};
use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRExprGenerator, IRExprResult, IRModuleProvider};

#[derive(Clone)]
pub struct Cond {
    pub cond: Expr,
    pub then_body: Block,
    pub else_body: Option<Block>,
}

impl Cond {
    #[inline]
    pub fn new(cond: Expr, then_body: Block, else_body: Option<Block>) -> Cond {
        Cond {
            cond: cond,
            then_body: then_body,
            else_body: else_body,
        }
    }
}

impl fmt::Debug for Cond {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}, {}", self.cond, self.then_body)?;
        if let Some(ref else_body) = self.else_body {
            write!(f, ", {}", else_body)?;
        }
        Ok(())
    }
}

pub fn parse_cond(parser: &mut Parser) -> Result<Cond, SyntaxError> {
    let cond = parse_expr(parser)?;

    // Parse then body with and without brackets
    let then_body = parse_block(parser)?;
    if let Some(&TokenType::Else) = parser.peek_type() {
        parser.tokens.pop(); // Eat 'else'

        // Parse else body with and without brackets
        let else_body = parse_block(parser)?;
        Ok(Cond::new(cond, then_body, Some(else_body)))
    } else {
        Ok(Cond::new(cond, then_body, None))
    }
}

impl IRExprGenerator for Cond {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRExprResult {
        // Calculates the condition
        let cond_expr = self.cond.gen_ir(context, module_provider)?;

        // Cast it to bool
        let bool_expr = types::cast_to(&self.cond.token, cond_expr, KoakType::Bool, context)?;

        // Compare it to zero
        let zero = IntConstRef::get(&IntTypeRef::get_int1(), 0, true).to_ref();
        let cond_res = context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, bool_expr.llvm_ref, zero, "condtmp");

        // Generate the new blocks
        let mut current_block = context.builder.get_insert_block();
        let mut function = current_block.get_parent();
        let mut then_block = function.append_basic_block_in_context(&mut context.context, "then");
        let mut merge_block = function.append_basic_block_in_context(&mut context.context, "merge");

        // Fill 'then' block
        context.push_scope();
        context.builder.position_at_end(&mut then_block);
        let then_value = self.then_body.gen_ir(context, module_provider);
        context.pop_scope();
        let then_value = then_value?;
        context.builder.build_br(&merge_block);
        let then_end = context.builder.get_insert_block();

        if let Some(ref else_body) = self.else_body {
            let mut else_block = function.append_basic_block_in_context(&mut context.context, "else");

            // Build bridge between 'then' and 'else' depending on the condition
            context.builder.position_at_end(&mut current_block);
            context.builder.build_cond_br(cond_res, &then_block, &else_block);

            // Fill 'else' block
            context.builder.position_at_end(&mut else_block);
            context.push_scope();
            let else_value = else_body.gen_ir(context, module_provider);
            context.pop_scope();
            let else_value = else_value?;
            context.builder.build_br(&merge_block);
            let else_end = context.builder.get_insert_block();

            // New code should be in the merge block
            context.builder.position_at_end(&mut merge_block);

            // Ensure 'if' / 'else' type value matche
            if then_value.ty == else_value.ty {
                if then_value.ty != KoakType::Void {
                    // Set the return value depend on the branch taken
                    let phi_type = then_value.ty;
                    let mut phi = unsafe {
                        PHINodeRef::from_ref(context.builder.build_phi(phi_type.as_llvm_ref(), "ifphi"))
                    };
                    phi.add_incoming(
                        vec![then_value.llvm_ref, else_value.llvm_ref].as_mut_slice(),
                        vec![then_end, else_end].as_mut_slice(),
                    );
                    Ok(KoakValue::new(phi.to_ref(), phi_type))
                } else {
                    Ok(KoakValue::new_void())
                }
            } else {
                Err(SyntaxError::from(&self.then_body.token, ErrorReason::IfBodiesTypeDoesntMatch(then_value.ty, else_value.ty)))
            }
        } else {
            // Build bridge between 'then' and 'else' depending on the condition
            context.builder.position_at_end(&mut current_block);
            context.builder.build_cond_br(cond_res, &then_block, &merge_block);

            // New code should be in the merge block
            context.builder.position_at_end(&mut merge_block);
            Ok(KoakValue::new_void())
        }
    }
}
