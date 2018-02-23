//!
//! Koak's blocks
//!

use std::fmt;

use lexer::{Token, TokenType};
use parser::Parser;
use lang::expr::{Expr, parse_expr};
use lang::cond::{Cond, parse_cond};
use lang::value::KoakValue;
use lang::for_loop::{ForLoop, parse_for_loop};
use codegen::{IRContext, IRModuleProvider, IRExprGenerator, IRExprResult};
use error::{SyntaxError, ErrorReason};

#[derive(Debug, Clone)]
pub enum BlockMember {
    Expr(Box<Expr>),
    Cond(Box<Cond>),
    ForLoop(Box<ForLoop>),
}

impl BlockMember {
    pub fn get_token(&self) -> &Token {
        match *self {
            BlockMember::Expr(ref e) => &e.token,
            BlockMember::Cond(ref c) => &c.cond.token,
            BlockMember::ForLoop(ref f) => &f.init.token,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub token: Token,
    exprs: Vec<BlockMember>,
    pub has_value: bool,
}

impl Block {
    #[inline]
    pub fn new(token: Token, bm: Vec<BlockMember>, value: bool) -> Block {
        Block {
            token: token,
            exprs: bm,
            has_value: value,
        }
    }

    pub fn from_member(token: Token, bm: BlockMember) -> Block {
        Block {
            token: token,
            exprs: vec![bm],
            has_value: true,
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{{")?;
        let mut first = true;
        for bm in &self.exprs {
            if !first {
                write!(f, ",")?;
            }
            write!(f, " {:?}", bm)?;
            first = false;
        }
        write!(f, " }}")
    }
}

pub fn parse_block_member(parser: &mut Parser) -> Result<BlockMember, SyntaxError> {
    let member = parser.peek_or(ErrorReason::ExprExpected)?.clone();
    match member.token_type {
        TokenType::If => {
            parser.tokens.pop(); // Eat 'if'
            Ok(BlockMember::Cond(Box::new(parse_cond(parser)?)))
        },
        TokenType::For => {
            parser.tokens.pop(); // Eat 'for'
            Ok(BlockMember::ForLoop(Box::new(parse_for_loop(parser)?)))
        },
        _ => Ok(BlockMember::Expr(Box::new(parse_expr(parser)?))),
    }
}

#[allow(unused_assignments)]
pub fn parse_block(parser: &mut Parser) -> Result<Block, SyntaxError> {
    let mut v = Vec::new();
    let mut close_bracket = Token::new();
    let mut has_value = false;

    // Parse block input
    loop {
        let member = parser.peek_or(ErrorReason::ExprExpected)?.clone();
        if let Some(&TokenType::CloseBracket) = parser.peek_type() {
            parser.tokens.pop(); // Eat '}'
            close_bracket = member;
            has_value = false;
            break
        }
        has_value = true;
        v.push(parse_block_member(parser)?);

        let delimiter = parser.next_or(ErrorReason::UnterminatedBlock)?;
        match delimiter.token_type {
            TokenType::SemiColon => continue,
            TokenType::CloseBracket => {
                close_bracket = delimiter;
                break
            },
            _ => return Err(SyntaxError::from(&delimiter, ErrorReason::ExpectedSemiColorOrCloseBracket))
        }
    }
    Ok(Block::new(close_bracket, v, has_value))
}

impl IRExprGenerator for Block {
    #[inline]
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRExprResult {

        let mut last = KoakValue::new_void();

        // Generate all bodies
        for bm in &self.exprs {
            last = match *bm {
                BlockMember::Expr(ref expr) => expr.gen_ir(context, module_provider)?,
                BlockMember::Cond(ref cond) => cond.gen_ir(context, module_provider)?,
                BlockMember::ForLoop(ref forloop) => forloop.gen_ir(context, module_provider)?,
            };
        }
        if self.has_value { // Blocks what end with a ';' are void-blocks
            Ok(last)
        } else {
            Ok(KoakValue::new_void())
        }
    }
}