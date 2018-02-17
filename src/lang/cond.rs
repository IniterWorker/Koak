//!
//! This module implements control-flow
//!

use std::fmt;

use iron_llvm::{LLVMRef, LLVMRefCtor};
use iron_llvm::core::instruction::{PHINode, PHINodeRef};
use iron_llvm::core::value::Function;
use iron_llvm::core::basic_block::*;

use lexer::TokenType;
use parser::Parser;
use lang::expr::{Expr, parse_expr};
use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRExprGenerator, IRExprResult, IRModuleProvider};

#[derive(Clone)]
pub struct Cond {
    cond: Expr,
    then_body: Expr,
    else_body: Expr,
}

impl Cond {
    #[inline]
    pub fn new(cond: Expr, then_body: Expr, else_body: Expr) -> Cond {
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
        write!(f, "{:?}, {:?}, {:?}", self.cond, self.then_body, self.else_body)
    }
}

pub fn parse_cond(parser: &mut Parser) -> Result<Cond, SyntaxError> {
    let cond = parse_expr(parser)?;
    let then_tok = parser.next_or(ErrorReason::ThenTokenExpected)?;
    match then_tok.token_type {
        TokenType::Then => {
            let then_body = parse_expr(parser)?;
            let else_tok = parser.next_or(ErrorReason::ElseTokenExpected)?;
            match else_tok.token_type {
                TokenType::Else => {
                    let else_body = parse_expr(parser)?;
                    Ok(Cond::new(cond, then_body, else_body))
                },
                _ => Err(SyntaxError::from(&else_tok, ErrorReason::ElseTokenExpected)),
            }
        },
        _ => Err(SyntaxError::from(&then_tok, ErrorReason::ThenTokenExpected)),
    }
}

impl IRExprGenerator for Cond {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRExprResult {
        // Calculate the condition
        let cond_expr = self.cond.gen_ir(context, module_provider)?;

        // Compare it to zero
        let cond_val = cond_expr.as_calculable().cmp_zero(context)?;

        // Generate the three new blocks
        let current_block = context.builder.get_insert_block();
        let mut function = current_block.get_parent();
        let mut then_block = function.append_basic_block_in_context(&mut context.context, "then");
        let mut else_block = function.append_basic_block_in_context(&mut context.context, "else");
        let mut merge_block = function.append_basic_block_in_context(&mut context.context, "merge");

        // Seperate the current_bloc in two
        context.builder.build_cond_br(cond_val.as_llvm_ref(), &then_block, &else_block);

        // Fill 'then' block
        context.builder.position_at_end(&mut then_block);
        let then_value = self.then_body.gen_ir(context, module_provider)?;
        context.builder.build_br(&merge_block);
        let then_end = context.builder.get_insert_block();

        // Fill 'else' block
        context.builder.position_at_end(&mut else_block);
        let else_value = self.else_body.gen_ir(context, module_provider)?;
        context.builder.build_br(&merge_block);
        let else_end = context.builder.get_insert_block();

        context.builder.position_at_end(&mut merge_block);

        // Ensure if and else return value matches
        if then_value.get_type() != else_value.get_type() {
            return Err(SyntaxError::from(&self.cond.token, ErrorReason::IfBodiesTypeDoesntMatch(then_value.get_type(), else_value.get_type())));
        }

        // Set the return value depdends on the branch taken
        let phi_type = then_value.get_type();
        let mut phi = unsafe {
            PHINodeRef::from_ref(context.builder.build_phi(phi_type.as_llvm_ref(), "ifphi"))
        };
        phi.add_incoming(
            vec![then_value.as_llvm_ref(), else_value.as_llvm_ref()].as_mut_slice(),
            vec![then_end, else_end].as_mut_slice(),
        );

        Ok(phi_type.new_value(phi.to_ref())?)
    }
}
