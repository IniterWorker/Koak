//!
//! Koak's for loop
//!

use std::fmt;
use std::rc::Rc;

use llvm_sys::LLVMIntPredicate;

use iron_llvm::{LLVMRef, LLVMRefCtor};
use iron_llvm::core::basic_block::BasicBlock;
use iron_llvm::core::{Function, Value};
use iron_llvm::core::instruction::{PHINode, PHINodeRef};
use iron_llvm::core::types::{IntTypeRef, IntTypeCtor};
use iron_llvm::core::value::{IntConstRef, IntConstCtor};

use lexer::TokenType;
use lang::expr::{parse_expr, Expr};
use lang::types::{cast_to, KoakType, KoakCalculable};
use parser::Parser;
use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRGenerator, IRResult, IRModuleProvider};

#[derive(Clone)]
pub struct ForLoop {
    var: Rc<String>,
    init: Expr,
    cond: Expr,
    step: Option<Expr>,
    body: Expr,
}

impl ForLoop {
    #[inline]
    pub fn new(var_name: Rc<String>, init: Expr, cond: Expr, step: Option<Expr>, body: Expr) -> ForLoop {
        ForLoop {
            var: var_name,
            init: init,
            cond: cond,
            step: step,
            body: body,
        }
    }
}

impl fmt::Debug for ForLoop {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}, {:?}, {:?}, {:?}", self.init, self.cond, self.step, self.body)
    }
}

pub fn parse_for_loop(parser: &mut Parser) -> Result<ForLoop, SyntaxError> {
    let iden = parser.next_or(ErrorReason::ForLoopIdentifierExpected)?;
    if let TokenType::Identifier(var) = iden.token_type {
        parser.next_of(TokenType::Equal, ErrorReason::ExpectedAssignmentAfterVarName)?;
        let init = parse_expr(parser)?;
        parser.next_of(TokenType::Comma, ErrorReason::ExpectedComma)?;
        let cond = parse_expr(parser)?;
        let mut inc = None;
        if let Some(&TokenType::Comma) = parser.peek_type() {
            parser.tokens.pop(); // Eat ','
            inc = Some(parse_expr(parser)?);
        }
        parser.next_of(TokenType::In, ErrorReason::ExpectedInAfterFor)?;
        let body = parse_expr(parser)?;
        Ok(ForLoop::new(var, init, cond, inc, body))
    } else {
        Err(SyntaxError::from(&iden, ErrorReason::ForLoopIdentifierExpected))
    }
}

impl IRGenerator for ForLoop {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRResult {
        let start_val = self.init.gen_ir(context, module_provider)?;

        let preloop_block = context.builder.get_insert_block();
        let mut function = preloop_block.get_parent();

        // Blocks used by our for-loop
        let mut loop_cond_bb = function.append_basic_block_in_context(&mut context.context, "forloop_cond");
        let mut loop_body_bb = function.append_basic_block_in_context(&mut context.context, "forloop_body");
        let mut loop_end_bb = function.append_basic_block_in_context(&mut context.context, "forloop_end");

        // Bridge the pre-loop block with the loop_cond block
        context.builder.build_br(&loop_cond_bb);

        // Put ourself at the beginning of the loop_cond block
        context.builder.position_at_end(&mut loop_cond_bb);

        // Create the init variable as a PHI node
        let mut phi_var = unsafe {
            PHINodeRef::from_ref(context.builder.build_phi(start_val.get_type(), &self.var))
        };

        // Start the PHI node with the start value
        phi_var.add_incoming(
            vec![start_val].as_mut_slice(),
            vec![preloop_block].as_mut_slice(),
        );

        // Push the init var in a new scope
        context.push_scope();
        context.add_var(self.var.clone(), phi_var.to_ref());

        let r  = do catch {

            // Compute the next value of the iteration variable
            let step_val = match &self.step {
                &Some(ref expr) => expr.gen_ir(context, module_provider)?,
                &None => IntConstRef::get(&IntTypeRef::get_int32(), 1, true).to_ref(),
            };
            let next_val = KoakCalculable::add(&phi_var.to_ref(), context, &self.init.token, step_val)?;

            // Compute the end condition
            let cond_expr = self.cond.gen_ir(context, module_provider)?;

            // Cast it to bool
            let bool_expr = cast_to(&self.cond.token, cond_expr, KoakType::Bool.as_llvm_ref(), context)?;

            // Compare it to zero
            let zero = IntConstRef::get(&IntTypeRef::get_int1(), 0, true).to_ref();
            let cond_res = context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, bool_expr, zero, "condtmp");

            // Create the conditional branches
            context.builder.build_cond_br(cond_res, &loop_body_bb, &loop_end_bb);

            // New code should be placed after the loop
            context.builder.position_at_end(&mut loop_body_bb);

            // Generate body
            self.body.gen_ir(context, module_provider)?;

            // Bridge the body to the loop block
            context.builder.build_br(&loop_cond_bb);

            // New code should be placed after the loop
            context.builder.position_at_end(&mut loop_end_bb);

            // Add a new incoming value for the PHI node
            phi_var.add_incoming(
                vec![next_val].as_mut_slice(),
                vec![loop_body_bb].as_mut_slice(),
            );

            Ok(phi_var.to_ref())
        };

        // Pop scope before eventually returning
        context.pop_scope();

        r
    }
}
