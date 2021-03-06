//!
//! Koak's for loop
//!

use std::fmt;
use std::rc::Rc;

use llvm_sys::LLVMIntPredicate;

use iron_llvm::LLVMRef;
use iron_llvm::core::basic_block::BasicBlock;
use iron_llvm::core::Function;
use iron_llvm::core::types::{IntTypeRef, IntTypeCtor};
use iron_llvm::core::value::{IntConstRef, IntConstCtor};

use lexer::{TokenType, OperatorType};
use lang::types::KoakType;
use lang::value::KoakValue;
use lang::expr::{Expr, parse_expr};
use lang::block::{Block, parse_block};
use parser::Parser;
use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRExprGenerator, IRExprResult, IRModuleProvider};

#[derive(Clone)]
pub struct ForLoop {
    pub var_name: Rc<String>,
    pub init: Expr,
    pub cond: Expr,
    pub step: Option<Expr>,
    pub body: Block,
}

impl ForLoop {
    #[inline]
    pub fn new(var_name: Rc<String>, init: Expr, cond: Expr, step: Option<Expr>, body: Block) -> ForLoop {
        ForLoop {
            var_name: var_name,
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
        let op = parser.next_or(ErrorReason::ExpectedAssignmentAfterVarName)?;
        if let TokenType::Operator(OperatorType::Assign) = op.token_type {
            let init = parse_expr(parser)?;
            parser.next_of(&TokenType::Comma, ErrorReason::ExpectedComma)?;
            let cond = parse_expr(parser)?;
            let mut inc = None;
            if let Some(&TokenType::Comma) = parser.peek_type() {
                parser.tokens.pop(); // Eat ','
                inc = Some(parse_expr(parser)?);
            }
            parser.next_of(&TokenType::In, ErrorReason::ExpectedInAfterFor)?;
            Ok(ForLoop::new(var, init, cond, inc, parse_block(parser)?))
        } else {
            Err(SyntaxError::from(&op, ErrorReason::ExpectedAssignmentAfterVarName))
        }
    } else {
        Err(SyntaxError::from(&iden, ErrorReason::ForLoopIdentifierExpected))
    }
}

impl IRExprGenerator for ForLoop {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRExprResult {
        context.push_scope();

        let r = do catch {
            // Generate the init value of the iteration variable
            let mut start_val = self.init.gen_ir(context, module_provider)?;

            let preloop_block = context.builder.get_insert_block();
            let mut function = preloop_block.get_parent();

            // Create the iteration variable, give it the starting value
            context.create_stack_var(&function, self.var_name.clone(), start_val);

            // Blocks used by our for-loop
            let mut loop_cond_bb = function.append_basic_block_in_context(&mut context.context, "forloop_cond");
            let mut loop_body_bb = function.append_basic_block_in_context(&mut context.context, "forloop_body");
            let mut loop_end_bb = function.append_basic_block_in_context(&mut context.context, "forloop_end");

            // Bridge the pre-loop block with the loop_cond block
            context.builder.build_br(&loop_cond_bb);

            // Put ourself at the beginning of the loop_cond block
            context.builder.position_at_end(&mut loop_cond_bb);

            // Compute the end condition and cast it to bool
            let cond_expr = self.cond.gen_ir(context, module_provider)?;
            let cond_bool = cond_expr.cast_to(&self.cond.token, context, KoakType::Bool)?;

            // Compare it to zero
            let zero = IntConstRef::get(&IntTypeRef::get_int1(), 0, true).to_ref();
            let cond_res = context.builder.build_icmp(LLVMIntPredicate::LLVMIntNE, cond_bool.llvm_ref, zero, "condtmp");

            // Create the conditional branches
            context.builder.build_cond_br(cond_res, &loop_body_bb, &loop_end_bb);

            // New code should be placed within the loop's body
            context.builder.position_at_end(&mut loop_body_bb);

            // Generate body
            context.push_scope();
            let r = self.body.gen_ir(context, module_provider);
            context.pop_scope();

            r?; // Proc errors

            // Compute the next value of the iteration variable
            let step_val = match self.step {
                Some(ref expr) => expr.gen_ir(context, module_provider)?,
                None => KoakValue::new(IntConstRef::get(&IntTypeRef::get_int32(), 1, true).to_ref(), KoakType::Int),
            };
            let new_val = context.get_local_var(&self.var_name).unwrap().add(context, &self.init.token, step_val)?;
            context.update_local_var(&self.init.token, self.var_name.clone(), new_val.llvm_ref)?;

            // Bridge the body to the loop block
            context.builder.build_br(&loop_cond_bb);

            // New code should be placed after the loop
            context.builder.position_at_end(&mut loop_end_bb);

            Ok(KoakValue::new_void())
        };
        context.pop_scope();
        r
    }
}
