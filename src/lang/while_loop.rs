//!
//! Koak's while loop
//!

use std::fmt;

use llvm_sys::LLVMIntPredicate;

use iron_llvm::LLVMRef;
use iron_llvm::core::basic_block::BasicBlock;
use iron_llvm::core::{Function};
use iron_llvm::core::types::{IntTypeRef, IntTypeCtor};
use iron_llvm::core::value::{IntConstRef, IntConstCtor};

use parser::Parser;
use error::SyntaxError;
use lang::expr::{Expr, parse_expr};
use lang::block::{Block, parse_block};
use lang::types::KoakType;
use lang::value::KoakValue;
use codegen::{IRContext, IRExprGenerator, IRExprResult, IRModuleProvider};

#[derive(Clone)]
pub struct WhileLoop {
    pub cond: Expr,
    pub body: Block,
}

impl WhileLoop {
    #[inline]
    pub fn new(cond: Expr, body: Block) -> WhileLoop {
        WhileLoop {
            cond: cond,
            body: body,
        }
    }
}

impl fmt::Debug for WhileLoop {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}, {:?}", self.cond, self.body)
    }
}

pub fn parse_while_loop(parser: &mut Parser) -> Result<WhileLoop, SyntaxError> {
    let cond = parse_expr(parser)?;
    let body = parse_block(parser)?;
    Ok(WhileLoop::new(cond, body))
}

impl IRExprGenerator for WhileLoop {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRExprResult {
        let preloop_block = context.builder.get_insert_block();
        let mut function = preloop_block.get_parent();

        // Blocks used by our while-loop
        let mut loop_cond_bb = function.append_basic_block_in_context(&mut context.context, "whileloop_cond");
        let mut loop_body_bb = function.append_basic_block_in_context(&mut context.context, "whileloop_body");
        let mut loop_end_bb = function.append_basic_block_in_context(&mut context.context, "whileloop_end");

        // Bridge the pre-loop block with the loop_cond block
        context.builder.build_br(&loop_cond_bb);

        // Put ourself at the beginning of the loop_cond block
        context.builder.position_at_end(&mut loop_cond_bb);

        // Compute the end condition and cast it to bool
        let cond_expr = self.cond.gen_ir(context, module_provider)?;
        let bool_expr = cond_expr.cast_to(&self.cond.token, context, KoakType::Bool)?;

        // Compare it to zero
        let zero = IntConstRef::get(&IntTypeRef::get_int1(), 0, true).to_ref();
        let cond_res = context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, bool_expr.llvm_ref, zero, "condtmp");

        // Create the conditional branches
        context.builder.build_cond_br(cond_res, &loop_body_bb, &loop_end_bb);

        // New code should be placed within the loop's body
        context.builder.position_at_end(&mut loop_body_bb);

        // Generate body, retain errors
        context.push_scope();
        let r = do catch {
            self.body.gen_ir(context, module_provider)
        };
        context.pop_scope();

        // Unleash errors
        r?;

        // Bridge the body to the loop block
        context.builder.build_br(&loop_cond_bb);

        // New code should be placed after the loop
        context.builder.position_at_end(&mut loop_end_bb);

        Ok(KoakValue::new_void())
    }
}
