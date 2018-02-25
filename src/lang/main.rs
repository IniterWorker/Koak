//!
//! Koak's main function
//!

use std::rc::Rc;

use lexer::Token;
use lang::block::{Block, BlockMember};
use lang::types::KoakType;
use lang::function::{FunctionPrototype, ConcreteFunction};
use codegen::{IRContext, IRModuleProvider, IRFuncGenerator, IRFuncResult};

pub struct MainFunction {
    pub exprs: Vec<BlockMember>,
}

impl MainFunction {
    pub fn new() -> MainFunction {
        MainFunction {
            exprs: Vec::new(),
        }
    }
}

impl IRFuncGenerator for MainFunction {
    #[inline]
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRFuncResult {
        // Generate content block
        let prot = Rc::new(FunctionPrototype::new(Token::new(), Rc::new("main".to_string()), Vec::new(), KoakType::Void));
        let main = ConcreteFunction::new(prot, Some(Block::new(Token::new(), self.exprs.clone(), false)));
        main.gen_ir(context, module_provider)
    }
}
