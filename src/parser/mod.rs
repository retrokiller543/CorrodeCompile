// Module: parser
// Purpose: parsing the input
// Path: src/parser/mod.rs

use crate::lexer::{Operator, Token};

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Program(Vec<ASTNode>),
    Statement(Box<ASTNode>),
    BinaryOp {
        op: Operator,
        left: Box<ASTNode>,
        right: Box<ASTNode>,
    },
    Assign {
        name: String,
        value: Box<ASTNode>,
    },
    Variable(String),
    Number(i32),
}

macro_rules! expect_token {
    ($self:ident, $token_variant:ident($val:ident) => $ast_node_variant:ident) => {
        match &$self.tokens[$self.position] { // Add reference here
            Token::$token_variant(ref $val) => { // Use ref keyword here
                $self.position += 1;
                Ok(ASTNode::$ast_node_variant($val.clone())) // Clone the value since it's a reference
            },
            _ => Err(format!("Expected a {}", stringify!($token_variant))),
        }
    };
    ($self:ident, $token_variant:ident => $ast_node_variant:ident) => {
        match $self.tokens[$self.position] {
            Token::$token_variant => {
                $self.position += 1;
                Ok(ASTNode::$ast_node_variant)
            },
            _ => Err(format!("Expected a {}", stringify!($token_variant))),
        }
    };
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

    pub fn parse(&mut self) -> Result<ASTNode, String> {
        let mut statements = Vec::new();

        while self.position < self.tokens.len() {
            let stmt = self.statement()?;
            statements.push(stmt);

            // Here, simply consume the EOL token and move to the next statement
            if let Some(Token::EOL) = self.peek_next_token() {
                self.position += 1; // Consume EOL
            } else {
                return Err("Expected end of line".to_string());
            }
        }

        Ok(ASTNode::Program(statements))
    }

    fn statement(&mut self) -> Result<ASTNode, String> {
        let stmt = match self.peek_next_token() {
            Some(Token::Number(_)) => self.expression(),
            Some(Token::Identifier(_)) => {
                if let Some(Token::Assign) = self.peek_nth_token(1) {
                    self.expect_assignment()
                } else {
                    self.expression()
                }
            }
            _ => Err("Expected a statement".to_string()),
        }?;

        Ok(ASTNode::Statement(Box::new(stmt)))
    }

    fn expression(&mut self) -> Result<ASTNode, String> {
        let mut left = self.term()?;

        while let Some(op) = self.peek_next_operator(&[Operator::Plus, Operator::Minus]) {
            self.position += 1; // Consume Operator
            let right = self.term()?;
            left = ASTNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<ASTNode, String> {
        let mut left = self.factor()?;

        while let Some(op) = self.peek_next_operator(&[Operator::Multiply, Operator::Divide]) {
            self.position += 1; // Consume Operator
            let right = self.factor()?;
            left = ASTNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<ASTNode, String> {
        if let Some(Token::Operator(Operator::Plus)) = self.peek_next_token() {
            self.position += 1; // Consume '+'
            return self.factor();
        }

        if let Some(Token::Operator(Operator::Minus)) = self.peek_next_token() {
            self.position += 1; // Consume '-'
            let right = self.primary()?;
            return Ok(ASTNode::BinaryOp {
                op: Operator::Minus,
                left: Box::new(ASTNode::Number(0)), // Represent as 0 - right
                right: Box::new(right),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<ASTNode, String> {
        // First, handle expressions enclosed in parentheses
        if let Some(Token::OpenParen) = self.peek_next_token() {
            self.position += 1; // Consume '('
            let expr = self.expression()?; // Evaluate the enclosed expression
            if let Some(Token::CloseParen) = self.peek_next_token() {
                self.position += 1; // Consume ')'
                return Ok(expr);
            } else {
                return Err("Expected closing parenthesis".to_string());
            }
        }

        // Next, handle numbers and variables
        match self.peek_next_token() {
            Some(Token::Number(_)) => self.expect_number(),
            Some(Token::Identifier(_)) => {
                if let Some(Token::Assign) = self.peek_nth_token(1) {
                    self.expect_assignment()
                } else {
                    self.expect_variable()
                }
            }
            // Here you can extend to handle other primary expressions
            _ => Err("Expected a number, variable, or primary expression".to_string()),
        }
    }

    fn peek_next_token(&self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            Some(&self.tokens[self.position])
        } else {
            None
        }
    }

    fn peek_next_operator(&self, ops: &[Operator]) -> Option<Operator> {
        if self.position >= self.tokens.len() {
            return None;
        }

        match self.tokens[self.position] {
            Token::Operator(ref op) if ops.contains(op) => Some(op.clone()),
            _ => None,
        }
    }

    fn peek_nth_token(&self, n: usize) -> Option<&Token> {
        if self.position + n < self.tokens.len() {
            Some(&self.tokens[self.position + n])
        } else {
            None
        }
    }

    fn expect_assignment(&mut self) -> Result<ASTNode, String> {
        let name = match &self.tokens[self.position] {
            Token::Identifier(ref ident) => ident.clone(),
            _ => return Err("Expected an identifier".to_string()),
        };
        self.position += 2; // Consume Identifier and Assign
        let value = self.expression()?;
        Ok(ASTNode::Assign {
            name,
            value: Box::new(value),
        })
    }

    fn expect_variable(&mut self) -> Result<ASTNode, String> {
        expect_token!(self, Identifier(name) => Variable)
    }

    fn expect_number(&mut self) -> Result<ASTNode, String> {
        expect_token!(self, Number(val) => Number)
    }
}

// get the tests from the tests module
#[cfg(test)]
mod tests;
