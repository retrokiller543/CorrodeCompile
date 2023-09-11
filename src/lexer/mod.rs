// Module: lexer
// Purpose: lexing the input
// Path: src/lexer/mod.rs

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Number(i32),
    Operator(Operator),
    Identifier(String),
    OpenParen,
    CloseParen,
    Assign,
    EOL,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
pub struct Lexer {
    input: String,
}

macro_rules! match_number {
    ($chars:ident) => {{
        let mut number = String::new();
        while let Some('0'..='9') = $chars.peek() {
            number.push($chars.next().unwrap());
        }
        if !number.is_empty() {
            Some(Token::Number(number.parse().unwrap()))
        } else {
            None
        }
    }};
}

macro_rules! match_identifier {
    ($chars:ident) => {{
        let mut identifier = String::new();
        while let Some(ch) = $chars.peek() {
            if ch.is_alphanumeric() || *ch == '_' {
                identifier.push($chars.next().unwrap());
            } else {
                break;
            }
        }
        if !identifier.is_empty() {
            Some(Token::Identifier(identifier))
        } else {
            None
        }
    }};
}

macro_rules! match_token {
    ($chars:ident, $( ($token_pattern: pat, $token_expr: expr) ),* , numbers, identifiers) => {
        match $chars.peek() {
            Some(&ch) => match ch {
                $($token_pattern => {
                    $chars.next();
                    $token_expr
                })*
                _ => match_number!($chars).or_else(|| match_identifier!($chars)),
            }
            _ => None,
        }
    };
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.to_string(),
        }
    }

    pub fn lex(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = self.input.chars().peekable();

        while chars.peek().is_some() {
            if let Some(token) = match_token!(chars,
                ('+', Some(Token::Operator(Operator::Plus))),
                ('-', Some(Token::Operator(Operator::Minus))),
                ('*', Some(Token::Operator(Operator::Multiply))),
                ('/', Some(Token::Operator(Operator::Divide))),
                ('=', Some(Token::Assign)),
                ('(', Some(Token::OpenParen)),
                (')', Some(Token::CloseParen)),
                (';', Some(Token::EOL)),
                numbers,
                identifiers
            ) {
                tokens.push(token);
            } else {
                chars.next(); // Skip unrecognized characters and whitespaces
            }
        }

        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_lexer {
        ($name:ident, $input:expr, $expected_output:expr) => {
            #[test]
            fn $name() {
                let lexer = Lexer::new($input);
                assert_eq!(lexer.lex(), $expected_output);
            }
        };
    }

    test_lexer!(empty, "", vec![]);

    test_lexer!(plus, "+", vec![Token::Operator(Operator::Plus)]);

    test_lexer!(minus, "-", vec![Token::Operator(Operator::Minus)]);

    test_lexer!(multiply, "*", vec![Token::Operator(Operator::Multiply)]);

    test_lexer!(divide, "/", vec![Token::Operator(Operator::Divide)]);

    test_lexer!(assign, "=", vec![Token::Assign]);

    test_lexer!(eol, ";", vec![Token::EOL]);

    test_lexer!(number, "123", vec![Token::Number(123)]);

    test_lexer!(identifier, "abc", vec![Token::Identifier("abc".to_string())]);

    test_lexer!(identifier_with_number, "abc123", vec![Token::Identifier("abc123".to_string())]);

    test_lexer!(identifier_with_underscore, "abc_123", vec![Token::Identifier("abc_123".to_string())]);

    test_lexer!(complex_expr, "variable_name = 123", vec![Token::Identifier("variable_name".to_string()), Token::Assign, Token::Number(123)]);
}