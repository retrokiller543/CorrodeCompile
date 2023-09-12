#[cfg(test)]

macro_rules! get_parser {
    ($input:expr) => {{
        let lexer = Lexer::new($input);
        let tokens = lexer.lex();
        Parser::new(tokens)
    }};
}

#[allow(unused_macros)]
macro_rules! assert_number {
    ($ast:expr, $expected_val:expr) => {
        match $ast {
            ASTNode::Program(stmts) => match stmts[0] {
                ASTNode::Statement(ref inner) => match **inner {
                    ASTNode::Number(val) => assert_eq!(val, $expected_val),
                    _ => panic!("Expected Number ASTNode"),
                },
                _ => panic!("Expected Statement ASTNode"),
            },
            _ => panic!("Expected Program ASTNode"),
        }
    };
}

#[allow(unused_macros)]
macro_rules! assert_binary_op {
    ($ast:expr, $expected_op:expr, $expected_left:expr, $expected_right:expr) => {
        match $ast {
            ASTNode::Program(stmts) => match stmts[0] {
                ASTNode::Statement(ref inner) => match **inner {
                    ASTNode::BinaryOp {
                        ref op,
                        ref left,
                        ref right,
                    } => {
                        assert_eq!(op, &$expected_op);
                        match (&**left, &**right) {
                            (&ASTNode::Number(l_val), &ASTNode::Number(r_val)) => {
                                assert_eq!(l_val, $expected_left);
                                assert_eq!(r_val, $expected_right);
                            }
                            _ => panic!("Expected Number ASTNodes for BinaryOp"),
                        }
                    }
                    _ => panic!("Expected BinaryOp ASTNode"),
                },
                _ => panic!("Expected Statement ASTNode"),
            },
            _ => panic!("Expected Program ASTNode"),
        }
    };
}

macro_rules! ast_test {
    ($name:ident, $input:expr, $body:expr) => {
        #[test]
        fn $name() {
            let ast = get_parser!($input).parse().unwrap();
            $body(&ast);
        }
    };
}

use crate::lexer::Lexer;
use crate::parser::*;

fn extract_number(ast: &ASTNode) -> Option<i32> {
    if let ASTNode::Number(val) = ast {
        return Some(*val);
    } else if let ASTNode::Program(stmts) = ast {
        if let ASTNode::Statement(ref inner) = &stmts[0] {
            if let ASTNode::Number(val) = **inner {
                return Some(val);
            }
        }
    }
    None
}

fn extract_binary_op(ast: &ASTNode) -> Option<(&Operator, &ASTNode, &ASTNode)> {
    match ast {
        ASTNode::BinaryOp {
            ref op,
            ref left,
            ref right,
        } => Some((op, left, right)),
        ASTNode::Program(stmts) => {
            if let ASTNode::Statement(ref inner) = &stmts[0] {
                if let ASTNode::BinaryOp {
                    ref op,
                    ref left,
                    ref right,
                } = **inner
                {
                    return Some((op, left, right));
                }
            }
            None
        }
        _ => None,
    }
}

ast_test!(test_single_number, "5;", |ast| {
    let num = extract_number(ast).expect("Expected a number");
    assert_eq!(num, 5);
});

ast_test!(test_binary_op, "5 + 3;", |ast| {
    let (op, left, right) = extract_binary_op(ast).expect("Expected a binary operation");
    assert_eq!(op, &Operator::Plus);
    assert_eq!(extract_number(left), Some(5));
    assert_eq!(extract_number(right), Some(3));
});

ast_test!(test_binary_op_precedence, "5 + 3 * 2;", |ast| {
    let (top_op, top_left, top_right) =
        extract_binary_op(ast).expect("Expected a top-level binary operation");
    assert_eq!(top_op, &Operator::Plus);
    assert_eq!(extract_number(top_left), Some(5));

    // Extract nested binary op from the right side of the top-level operation
    let (nested_op, nested_left, nested_right) =
        extract_binary_op(top_right).expect("Expected a nested binary operation");
    assert_eq!(nested_op, &Operator::Multiply);
    assert_eq!(extract_number(nested_left), Some(3));
    assert_eq!(extract_number(nested_right), Some(2));
});

ast_test!(test_large_expression, "5 + 3 * 2 - 1 / 4;", |ast| {
    let (top_op, top_left, top_right) =
        extract_binary_op(ast).expect("Expected a top-level binary operation");
    assert_eq!(top_op, &Operator::Minus);

    // Extract the Plus operation from the left side of the Minus operation
    let (plus_op, plus_left, plus_right) =
        extract_binary_op(top_left).expect("Expected a Plus binary operation");
    assert_eq!(plus_op, &Operator::Plus);
    assert_eq!(extract_number(plus_left), Some(5));

    // Extract the Multiply operation from the right side of the Plus operation
    let (multiply_op, multiply_left, multiply_right) =
        extract_binary_op(plus_right).expect("Expected a Multiply binary operation");
    assert_eq!(multiply_op, &Operator::Multiply);
    assert_eq!(extract_number(multiply_left), Some(3));
    assert_eq!(extract_number(multiply_right), Some(2));

    // Extract the Divide operation from the right side of the Minus operation
    let (divide_op, divide_left, divide_right) =
        extract_binary_op(top_right).expect("Expected a Divide binary operation");
    assert_eq!(divide_op, &Operator::Divide);
    assert_eq!(extract_number(divide_left), Some(1));
    assert_eq!(extract_number(divide_right), Some(4));
});
