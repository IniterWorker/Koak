//!
//! Koak's parser
//!

use lexer::{Token, TokenType};
use error::{SyntaxError, ErrorReason};
use lang::expr::{Expr, parse_expr};
use lang::func::{Func, parse_func_def};
use lang::extern_func::{ExternFunc, parse_extern_func};
use codegen::{IRContext, IRGenerator, IRModuleProvider, IRResult};

pub type ParserResult = Result<ASTNode, SyntaxError>;

///
/// All root declarations possibles.
///
#[derive(Debug)]
pub enum ASTNode {
    ExternProto(ExternFunc),
    FuncDef(Func),
    TopLevelExpr(Expr),
}

impl IRGenerator for ASTNode {
    #[inline]
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRResult {
        match self {
            &ASTNode::ExternProto(ref proto) => proto.gen_ir(context, module_provider),
            &ASTNode::FuncDef(ref func) => func.gen_ir(context, module_provider),
            &ASTNode::TopLevelExpr(ref expr) => Func::new_anonymous((*expr).clone()).gen_ir(context, module_provider),
        }
    }
}

///
/// The parser is basically a structure holding common used items and helper functions.
/// Most of the parsing are done in the core language modules (in lang::*).
///
pub struct Parser {
    pub tokens: Vec<Token>,
    pub last_tok: Token,
}

impl Parser {
    #[inline]
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        tokens.reverse();
        let last_tok = tokens.first().map(|t| t.clone()).unwrap_or(Token::new());
        Parser {
            tokens: tokens, // Reverse so that the current token is the last one
            last_tok: last_tok,
        }
    }

    #[inline]
    pub fn peek_type(&self) -> Option<&TokenType> {
        self.tokens.last().map(|x| &x.token_type)
    }

    #[inline]
    pub fn next_or(&mut self, er: ErrorReason) -> Result<Token, SyntaxError> {
        self.tokens.pop().ok_or(SyntaxError::new(er, self.last_tok.line.clone(), self.last_tok.row, self.last_tok.col))
    }

    #[inline]
    pub fn peek_or(&self, er: ErrorReason) -> Result<&Token, SyntaxError> {
        self.tokens.last().ok_or(SyntaxError::new(er, self.last_tok.line.clone(), self.last_tok.row, self.last_tok.col))
    }

    #[inline]
    fn parse_function_def(&mut self) -> Result<ASTNode, SyntaxError> {
        Ok(ASTNode::FuncDef(parse_func_def(self)?))
    }

    #[inline]
    fn parse_extern_declaration(&mut self) -> Result<ASTNode, SyntaxError> {
        Ok(ASTNode::ExternProto(parse_extern_func(self)?))
    }

    #[inline]
    fn parse_toplevel_expr(&mut self) -> Result<ASTNode, SyntaxError> {
        let expr = parse_expr(self)?;
        Ok(ASTNode::TopLevelExpr(expr))
    }
}

impl Iterator for Parser {
    type Item = ParserResult;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.peek_type()? {
            &TokenType::Def => Some(self.parse_function_def()),
            &TokenType::Extern => Some(self.parse_extern_declaration()),
            _ => Some(self.parse_toplevel_expr())
        }
    }
}
