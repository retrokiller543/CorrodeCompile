// Module: assembly_generator
// Purpose: generate assembly from AST
// Path: src/assembly_generator/mod.rs

use crate::lexer::{Lexer, Operator};
use crate::parser::{ASTNode, Parser};
use std::collections::HashMap;

macro_rules! asm_push {
    ($value:expr) => {
        format!("push {}\n", $value)
    };
}

macro_rules! asm_pop {
    ($register:expr) => {
        format!("pop {}\n", $register)
    };
}

macro_rules! asm_mov {
    ($dst:expr, $src:expr) => {
        format!("mov [rsp{}], {}\n", $dst, $src)
    };
}

macro_rules! asm_binary_op {
    ($op:tt) => {
        format!(
            "pop rdi\n\
                 pop rax\n\
                 {} rax, rdi\n\
                 push rax\n",
            stringify!($op)
        )
    };
}

struct SymbolTable {
    variables: HashMap<String, isize>, // Using isize for stack offsets
    next_offset: isize,
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable {
            variables: HashMap::new(),
            next_offset: 0, // Starting offset for variables on the stack
        }
    }

    fn allocate(&mut self) -> isize {
        self.next_offset -= 8; // each variable takes up 8 bytes on a 64-bit system
        self.next_offset
    }

    fn store_variable(&mut self, name: String) -> isize {
        let offset = self.allocate();
        self.variables.insert(name, offset);
        offset
    }

    fn get_offset(&self, name: &str) -> Option<isize> {
        self.variables.get(name).copied()
    }
}

fn generate_assembly(ast: &ASTNode, sym_table: &mut SymbolTable) -> String {
    match ast {
        ASTNode::Program(statements) => {
            let mut program_asm = String::new();
            for stmt in statements {
                program_asm += &generate_assembly(stmt, sym_table);
            }
            program_asm
        }
        ASTNode::Statement(inner) => generate_assembly(inner, sym_table),
        ASTNode::Number(val) => {
            asm_push!(val)
        }
        ASTNode::BinaryOp { op, left, right } => {
            let left_asm = generate_assembly(left, sym_table);
            let right_asm = generate_assembly(right, sym_table);
            let op_asm = match op {
                Operator::Plus => asm_binary_op!(add),
                Operator::Minus => asm_binary_op!(sub),
                Operator::Multiply => asm_binary_op!(imul),
                Operator::Divide => "pop rdi\n\
                             pop rax\n\
                             cqo\n\
                             idiv rdi\n\
                             push rax\n"
                    .to_string(),
            };
            format!("{}{}{}", left_asm, right_asm, op_asm)
        }
        ASTNode::Variable(name) => {
            if let Some(offset) = sym_table.get_offset(name) {
                format!("mov rax, [rsp{}]\npush rax\n", offset)
            } else {
                format!("; Variable {} not found\n", name)
            }
        }

        ASTNode::Assign { name, value } => {
            let val_asm = generate_assembly(value, sym_table);
            let offset = if let Some(existing_offset) = sym_table.get_offset(name) {
                existing_offset
            } else {
                sym_table.store_variable(name.clone())
            };
            format!("{}pop rax\nmov [rsp{}], rax\n", val_asm, offset)
        }
    }
}

fn wrap_as_program(assembly: &str, sym_table: &SymbolTable) -> String {
    let mut program = String::new();
    program += "section .text\n";
    program += "\tglobal _start\n\n";
    program += "_start:\n";
    program += "push rbp\n"; // Save the current base pointer
    program += "mov rbp, rsp\n"; // Set the new base pointer
    program += "sub rsp, "; // Reserve space for 4 local variables (assuming a maximum for this example)
    program += &(sym_table.variables.len() * 8).to_string();
    program += "\n\n";
    program += "; user code\n";
    program += assembly;
    program += "\n";
    program += "mov rsp, rbp\n"; // Restore the stack pointer
    program += "pop rbp\n"; // Restore the base pointer
    program += "; exit program\n";
    program += "mov rdi, rax\n"; // Set the exit status to the value in rax

    program += "mov rax, 60\n"; // Syscall: exit
    program += "syscall\n"; // invoke syscall
    program
}

pub fn compile_to_assembly(input: &str) -> String {
    let lexer = Lexer::new(input);
    let tokens = lexer.lex();

    let mut parser = Parser::new(tokens);
    let ast_nodes = parser.parse().expect("Failed to parse");
    dbg!(&ast_nodes.clone());

    let mut sym_table = SymbolTable::new();
    let mut assembly = String::new();

    assembly += &generate_assembly(&ast_nodes, &mut sym_table);

    wrap_as_program(&assembly, &sym_table)
}
