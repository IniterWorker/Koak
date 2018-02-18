//!
//! Koak's parser
//!

use std::rc::Rc;

use lexer::{Token, TokenType};
use error::{SyntaxError, ErrorReason};
use lang::expr::{Expr, parse_expr};
use lang::function::{ConcreteFunction, parse_func_def, parse_extern_func};
use codegen::{IRContext, IRGenerator, IRModuleProvider, IRResult};
use pipeline::module;

pub type ParserResult = Result<Vec<ASTNode>, Vec<SyntaxError>>;

///
/// All root declarations possibles.
///
#[derive(Debug)]
pub enum ASTNode {
    FunctionDef(Rc<ConcreteFunction>),
    TopLevelExpr(Expr),
}

impl IRGenerator for ASTNode {
    #[inline]
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRResult {
        match self {
            &ASTNode::FunctionDef(ref func) => {
                context.push_scope();
                context.functions.insert(func.name.clone(), func.clone());
                let r = func.gen_ir(context, module_provider);
                if r.is_err() {
                    context.functions.remove(&*func.name);
                }
                context.pop_scope();
                r
            }
            &ASTNode::TopLevelExpr(ref expr) => {
                ConcreteFunction::new_anonymous((*expr).clone()).gen_ir(context, module_provider)
            }
        }
    }
}

///
/// The parser is basically a structure holding common used items and helper functions.
/// Most of the parsing are done in the core language modules (in lang::*).
///
pub struct Parser<'a> {
    pub module_manager: &'a mut module::ModuleManager,
    pub tokens: Vec<Token>,
    pub last_tok: Token,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(mm: &'a mut module::ModuleManager, mut tokens: Vec<Token>) -> Parser<'a> {
        tokens.reverse();
        let last_tok = tokens.first().map(|t| t.clone()).unwrap_or(Token::new());
        Parser {
            module_manager: mm,
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
    pub fn next_of(&mut self, ty: TokenType, er: ErrorReason) -> Result<Token, SyntaxError> {
        let token = self.next_or(er.clone())?;
        if token.token_type == ty {
            Ok(token)
        } else {
            Err(SyntaxError::from(&token, er))
        }
    }

    #[inline]
    pub fn peek_or(&self, er: ErrorReason) -> Result<&Token, SyntaxError> {
        self.tokens.last().ok_or(SyntaxError::new(er, self.last_tok.line.clone(), self.last_tok.row, self.last_tok.col))
    }

    #[inline]
    fn skip_until_semicolon(&mut self) {
        while let Some(t) = self.tokens.pop() {
            if let TokenType::SemiColon = t.token_type {
                break
            }
        }
    }

    #[inline]
    fn parse_function_def(&mut self) -> Result<ASTNode, SyntaxError> {
        Ok(ASTNode::FunctionDef(Rc::new(parse_func_def(self)?)))
    }

    #[inline]
    fn parse_extern_declaration(&mut self) -> Result<ASTNode, SyntaxError> {
        Ok(ASTNode::FunctionDef(Rc::new(parse_extern_func(self)?)))
    }

    fn parse_import(&mut self) -> ParserResult {
        self.tokens.pop(); // Eat 'import'
        let t = self.next_or(ErrorReason::ModuleNameExpected).map_err(|e| vec![e])?;
        if let TokenType::StringLitteral(ref s) = t.token_type {
            self.next_of(TokenType::SemiColon, ErrorReason::MissingSemiColonAfterImport).map_err(|e| vec![e])?;
            module::load_module(self.module_manager, &t, s)
        } else {
            Err(vec![SyntaxError::from(&t, ErrorReason::ModuleNameExpected)])
        }
    }

    #[inline]
    fn parse_toplevel_expr(&mut self) -> Result<ASTNode, SyntaxError> {
        let expr = parse_expr(self)?;

        let colon = self.next_or(ErrorReason::MissingSemiColonAfterTopLevelExpr)?;
        if let TokenType::SemiColon = colon.token_type { // Check for semi-colon
            Ok(ASTNode::TopLevelExpr(expr))
        } else {
            Err(SyntaxError::from(&colon, ErrorReason::MissingSemiColonAfterTopLevelExpr))
        }
    }

}

impl<'a> Iterator for Parser<'a> {
    type Item = ParserResult;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&TokenType::SemiColon) = self.peek_type() { // Skip semi-colon
            self.tokens.pop();
        }

        let out = match self.peek_type()? {
            &TokenType::Def => self.parse_function_def(),
            &TokenType::Extern => self.parse_extern_declaration(),
            &TokenType::Import => {
                return Some(self.parse_import());
            },
            _ => self.parse_toplevel_expr()
        };

        Some(out
            .and_then(|o| {
                Ok(vec![o])
            })
            .or_else(|e| { // Skip tokens until semi-colon, to avoid multiple errors
                self.skip_until_semicolon();
                Err(vec![e])
            })
        )
    }
}
