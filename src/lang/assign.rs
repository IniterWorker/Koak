//!
//! Koak's assigniation
//!

use std::fmt;
use std::rc::Rc;

use iron_llvm::core::basic_block::BasicBlock;

use parser::Parser;
use lexer::{Token, TokenType, OperatorType};
use lang::block::{Block, parse_block};
use lang::types::KoakType;
use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRModuleProvider, IRExprGenerator, IRExprResult};

#[derive(Clone)]
pub struct LetAssign {
    pub token: Token,
    pub name: Rc<String>,
    pub value: Block,
    pub is_mut: bool,
}

impl LetAssign {
    #[inline]
    pub fn new(token: Token, name: Rc<String>, value: Block, is_mut: bool) -> LetAssign {
        LetAssign {
            token: token,
            name: name,
            value: value,
            is_mut: is_mut,
        }
    }
}

impl fmt::Debug for LetAssign {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}{} = {}",
            if self.is_mut { "mut " } else { "" },
            self.name,
            self.value,
        )
    }
}

pub fn parse_let_assign(parser: &mut Parser) -> Result<LetAssign, SyntaxError> {
    let mut is_mut = false;
    if let Some(&TokenType::Mut) = parser.peek_type() {
        parser.tokens.pop(); // Eat 'mut'
        is_mut = true;
    }
    let iden = parser.next_or(ErrorReason::ExpectedAssignmentVarName)?;
    if let TokenType::Identifier(ref name) = iden.token_type {
        let op = parser.next_or(ErrorReason::ExpectedAssignmentAfterVarName)?;
        if let TokenType::Operator(OperatorType::Assign) = op.token_type {
            Ok(LetAssign::new(op, name.clone(), parse_block(parser)?, is_mut))
        } else {
            Err(SyntaxError::from(&op, ErrorReason::ExpectedAssignmentAfterVarName))
        }
    } else {
        Err(SyntaxError::from(&iden, ErrorReason::ExpectedAssignmentVarName))
    }
}

impl IRExprGenerator for LetAssign {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRExprResult {
        // Generate value
        context.push_scope();
        let val = self.value.gen_ir(context, module_provider);
        context.pop_scope();

        let mut val = val?;

        if val.ty != KoakType::Void {
            let current_block = context.builder.get_insert_block();
            let func = current_block.get_parent();
            val.is_mut = self.is_mut;
            if context.toplevel {
                Err(SyntaxError::from(&self.token, ErrorReason::TopLevelAssignForbidden))
            } else {
                context.create_stack_var(&func, self.name.clone(), val.clone());
                Ok(val)
            }
        }
        else {
            Err(SyntaxError::from(&self.token, ErrorReason::CantAssignVoidValue))
        }
    }
}
