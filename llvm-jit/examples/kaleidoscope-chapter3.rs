#[macro_use]
extern crate log;
extern crate pretty_env_logger;
#[macro_use]
extern crate error_chain;
extern crate rustyline;
#[macro_use]
extern crate llvm_jit as jit;

mod errors {
    use jit;

    use lexer::Token;

    error_chain!{
        foreign_links {

        }

        links {
            Jit(jit::errors::Error, jit::errors::ErrorKind);
        }

        errors {
            UnexpectedToken(msg: String, token: Token) {
                description("unexpected token")
                display("{}, but got unexpected token: '{:?}'", msg, token)
            }
            UnknownVariable(name: String) {
                description("unknown variable")
                display("unknown variable: {}", name)
            }
            UnknownFunction(name: String) {
                description("unknown function")
                display("unknown function: {}", name)
            }
            IncorrectArguments(passed: usize, expected: usize) {
                description("incorrect arguments passed")
                display("incorrect arguments, passed {}, expected {}", passed, expected)
            }
        }
    }
}

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

mod lexer {
    // The lexer returns tokens [0-255] if it is an unknown character, otherwise one
    // of these for known things.
    #[derive(Clone, Debug, PartialEq)]
    pub enum Token {
        Eof,

        // commands
        Def,
        Extern,

        // primary
        Identifier(String),
        Number(f64),
        Character(char),
        Comment(String),
    }

    pub struct Lexer<I: Iterator> {
        iter: Backable<I>,
    }

    impl<I: Iterator> Lexer<I> {
        pub fn new(iter: I) -> Self {
            Lexer { iter: iter.backable() }
        }
    }

    impl<I> Iterator for Lexer<I>
    where
        I: Iterator<Item = char>,
    {
        type Item = Token;

        fn next(&mut self) -> Option<Self::Item> {
            match self.gettok() {
                Token::Eof => None,
                token => Some(token),
            }
        }
    }

    impl<I> Lexer<I>
    where
        I: Iterator<Item = char>,
    {
        /// gettok - Return the next token from standard input.
        pub fn gettok(&mut self) -> Token {
            // Skip any whitespace.
            let c = self.iter.by_ref().skip_while(|c| c.is_whitespace()).next();

            if let Some(c) = c {
                if c.is_alphabetic() {
                    self.iter.step_back();

                    // identifier: [a-zA-Z][a-zA-Z0-9]*
                    let s: String = self.iter
                        .by_ref()
                        .take_while(|c| c.is_alphanumeric())
                        .collect();

                    self.iter.step_back();

                    match s.as_str() {
                        "def" => Token::Def,
                        "extern" => Token::Extern,
                        _ => Token::Identifier(s),
                    }
                } else if c.is_digit(10) || c == '.' {
                    self.iter.step_back();

                    // number: [0-9.]+
                    let s: String = self.iter
                        .by_ref()
                        .take_while(|&c| c.is_digit(10) || c == '.')
                        .collect();

                    self.iter.step_back();

                    Token::Number(s.parse().unwrap())
                } else if c == '#' {
                    // Comment until end of line.
                    let s: String = self.iter
                        .by_ref()
                        .take_while(|&c| c != '\n' && c != '\r')
                        .collect();

                    Token::Comment(s.to_owned())
                } else {
                    // Otherwise, just return the character as its ascii value.
                    Token::Character(c)
                }
            } else {
                // Check for end of file.  Don't eat the EOF.
                Token::Eof
            }
        }
    }

    trait BackableIterator<I: Iterator> {
        fn backable(self) -> Backable<I>;
    }

    impl<I: Iterator> BackableIterator<I> for I {
        fn backable(self) -> Backable<I> {
            Backable::new(self)
        }
    }

    struct Backable<I: Iterator> {
        iter: I,
        front: Option<Option<I::Item>>,
        back: Option<Option<I::Item>>,
    }

    impl<I: Iterator> Backable<I> {
        pub fn new(iter: I) -> Backable<I> {
            Backable {
                iter,
                front: None,
                back: None,
            }
        }

        pub fn step_back(&mut self) {
            if self.back.is_some() {
                self.front = self.back.take();
            }
        }
    }

    impl<I: Iterator> Iterator for Backable<I>
    where
        I::Item: Clone,
    {
        type Item = I::Item;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(item) = self.front.take() {
                item
            } else {
                self.back = Some(self.iter.next());

                if let Some(ref item) = self.back {
                    item.clone()
                } else {
                    None
                }
            }
        }
    }
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

mod ast {
    use std::fmt;

    use jit::prelude::*;

    use codegen::CodeGenerator;
    use errors::Result;
    use lexer::Token;

    #[derive(Debug, PartialEq, PartialOrd)]
    pub enum BinOp {
        LessThen = 10,
        Add = 20,
        Sub = 30,
        Mul = 40,
    }

    impl Token {
        pub fn as_bin_op(&self) -> Option<BinOp> {
            match *self {
                Token::Character('<') => Some(BinOp::LessThen),
                Token::Character('+') => Some(BinOp::Add),
                Token::Character('-') => Some(BinOp::Sub),
                Token::Character('*') => Some(BinOp::Mul),
                _ => None,
            }
        }
    }

    /// Expr - Base class for all expression nodes.
    pub trait Expr: fmt::Debug {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef>;
    }

    /// NumberExpr - Expression class for numeric literals like "1.0".
    #[derive(Debug)]
    pub struct NumberExpr {
        pub val: f64,
    }

    /// VariableExpr - Expression class for referencing a variable, like "a".
    #[derive(Debug)]
    pub struct VariableExpr {
        pub name: String,
    }

    /// BinaryExpr - Expression class for a binary operator.
    #[derive(Debug)]
    pub struct BinaryExpr {
        pub op: BinOp,
        pub lhs: Box<Expr>,
        pub rhs: Box<Expr>,
    }

    /// CallExpr - Expression class for function calls.
    #[derive(Debug)]
    pub struct CallExpr {
        pub callee: String,
        pub args: Vec<Box<Expr>>,
    }

    /// Prototype - This class represents the "prototype" for a function,
    /// which captures its name, and its argument names (thus implicitly the number
    /// of arguments the function takes).
    #[derive(Debug)]
    pub struct Prototype {
        pub name: String,
        pub args: Vec<String>,
    }

    /// FunctionAST - This class represents a function definition itself.
    #[derive(Debug)]
    pub struct Function {
        pub proto: Prototype,
        pub body: Box<Expr>,
    }
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

mod parser {
    use ast;
    use errors::{ErrorKind, Result};
    use lexer::{Lexer, Token};

    pub struct Parser<I>
    where
        I: Iterator,
    {
        lexer: Lexer<I>,
        /// the current token the parser is looking at.
        cur_token: Token,
    }

    pub fn new<I>(iter: I) -> Parser<I>
    where
        I: Iterator,
    {
        Parser {
            lexer: Lexer::new(iter),
            cur_token: Token::Eof,
        }
    }

    impl<I> Parser<I>
    where
        I: Iterator<Item = char>,
    {
        /// reads another token from the lexer and updates `cur_token` with its results.
        pub fn next_token(&mut self) -> &Token {
            self.cur_token = self.lexer.next().unwrap_or(Token::Eof);

            trace!("parsed token: {:?}", self.cur_token);

            &self.cur_token
        }

        /// Get the precedence of the pending binary operator token.
        fn get_tok_precedence(&self) -> Option<i32> {
            if let Some(op) = self.cur_token.as_bin_op() {
                Some(op as i32)
            } else {
                None
            }
        }

        /// numberexpr ::= number
        fn parse_number_expr(&mut self) -> Result<Box<ast::Expr>> {
            match self.cur_token {
                Token::Number(val) => {
                    self.next_token(); // consume the number

                    Ok(Box::new(ast::NumberExpr { val }))
                }
                ref token => {
                    bail!(ErrorKind::UnexpectedToken(
                        "Expected `number`".into(),
                        token.clone(),
                    ));
                }
            }
        }

        /// parenexpr ::= '(' expression ')'
        fn parse_paren_expr(&mut self) -> Result<Box<ast::Expr>> {
            match self.cur_token {
                Token::Character('(') => {
                    self.next_token(); // eat (.

                    let expr = self.parse_expression();

                    match self.cur_token {
                        Token::Character(')') => {
                            self.next_token(); // eat ).

                            expr
                        }
                        ref token => {
                            bail!(ErrorKind::UnexpectedToken(
                                "Expected `)`".into(),
                                token.clone(),
                            ));
                        }
                    }
                }
                ref token => {
                    bail!(ErrorKind::UnexpectedToken(
                        "Expected `(`".into(),
                        token.clone(),
                    ));
                }
            }
        }

        /// identifierexpr
        ///   ::= identifier
        ///   ::= identifier '(' expression* ')'
        fn parse_identifier_expr(&mut self) -> Result<Box<ast::Expr>> {
            let name = match self.cur_token {
                Token::Identifier(ref name) => name.clone(),
                ref token => {
                    bail!(ErrorKind::UnexpectedToken(
                        "Expected `identifier`".into(),
                        token.clone(),
                    ));
                }
            };

            self.next_token(); // eat identifier.

            match self.cur_token {
                Token::Character('(') => {
                    self.next_token(); // eat (

                    let mut args = vec![];

                    loop {
                        args.push(self.parse_expression()?);

                        match self.cur_token {
                            Token::Character(')') => {
                                self.next_token(); // Eat the ')'.

                                break;
                            }
                            Token::Character(',') => {
                                self.next_token(); // Eat the ','.

                                continue;
                            }
                            ref token => {
                                bail!(ErrorKind::UnexpectedToken(
                                    "Expected `)` or `,` in argument list".into(),
                                    token.clone(),
                                ));
                            }
                        }
                    }

                    Ok(Box::new(ast::CallExpr { callee: name, args }))
                }
                _ => Ok(Box::new(ast::VariableExpr { name: name.clone() })),
            }
        }

        /// primary
        ///   ::= identifierexpr
        ///   ::= numberexpr
        ///   ::= parenexpr
        fn parse_primary(&mut self) -> Result<Box<ast::Expr>> {
            match self.cur_token {
                Token::Identifier(_) => self.parse_identifier_expr(),
                Token::Number(_) => self.parse_number_expr(),
                Token::Character('(') => self.parse_paren_expr(),
                ref token => {
                    bail!(ErrorKind::UnexpectedToken(
                        "Expected `identifier`, `number` or `(`".into(),
                        token.clone(),
                    ));
                }
            }
        }

        /// binoprhs
        ///   ::= ('+' primary)*
        fn parse_bin_op_rhs(
            &mut self,
            expr_prec: i32,
            mut lhs: Box<ast::Expr>,
        ) -> Result<Box<ast::Expr>> {
            // If this is a binop, find its precedence.
            loop {
                let tok_prec = self.get_tok_precedence().unwrap_or(-1);

                // If this is a binop that binds at least as tightly as the current binop,
                // consume it, otherwise we are done.
                if tok_prec < expr_prec {
                    return Ok(lhs);
                }

                // Okay, we know this is a binop.
                let op = self.cur_token.as_bin_op().unwrap();

                self.next_token(); // eat binop

                // Okay, we know this is a binop.
                let mut rhs = self.parse_primary()?;

                // If BinOp binds less tightly with RHS than the operator after RHS,
                // let the pending operator take RHS as its LHS.
                let next_prec = self.get_tok_precedence().unwrap_or(-1);

                if tok_prec < next_prec {
                    rhs = self.parse_bin_op_rhs(tok_prec + 1, rhs)?;
                }

                lhs = Box::new(ast::BinaryExpr { op, lhs, rhs })
            }
        }

        /// expression
        ///   ::= primary binoprhs
        ///
        fn parse_expression(&mut self) -> Result<Box<ast::Expr>> {
            let lhs = self.parse_primary()?;

            self.parse_bin_op_rhs(0, lhs)
        }

        /// prototype
        ///   ::= id '(' id* ')'
        fn parse_prototype(&mut self) -> Result<ast::Prototype> {
            let name = match self.cur_token {
                Token::Identifier(ref name) => name.clone(),
                ref token => {
                    bail!(ErrorKind::UnexpectedToken(
                        "Expected function name in prototype".into(),
                        token.clone(),
                    ))
                }
            };

            match *self.next_token() {  // Eat the `id`
                Token::Character('(') => {
                    // ignore it
                }
                ref token => {
                    bail!(ErrorKind::UnexpectedToken(
                        "Expected `(` in prototype".into(),
                        token.clone(),
                    ))
                }
            }

            let mut args = vec![];

            while let &Token::Identifier(ref name) = self.next_token() {
                args.push(name.clone());
            }

            match self.cur_token {
                Token::Character(')') => {
                    self.next_token(); // eat ')'.
                }
                ref token => {
                    bail!(ErrorKind::UnexpectedToken(
                        "Expected `)` in prototype".into(),
                        token.clone(),
                    ))
                }
            }

            Ok(ast::Prototype { name, args })
        }

        /// definition ::= 'def' prototype expression
        pub fn parse_definition(&mut self) -> Result<ast::Function> {
            match self.cur_token {
                Token::Def => {
                    self.next_token(); // eat def.
                }
                ref token => {
                    bail!(ErrorKind::UnexpectedToken(
                        "Expected `def` in definition".into(),
                        token.clone(),
                    ))
                }
            }

            let proto = self.parse_prototype()?;
            let body = self.parse_expression()?;

            Ok(ast::Function { proto, body })
        }

        /// toplevelexpr ::= expression
        pub fn parse_top_level_expr(&mut self) -> Result<ast::Function> {
            let expr = self.parse_expression()?;

            Ok(ast::Function {
                proto: ast::Prototype {
                    name: "__anon_expr".into(),
                    args: vec![],
                },
                body: expr,
            })
        }

        /// external ::= 'extern' prototype
        pub fn parse_extern(&mut self) -> Result<ast::Prototype> {
            self.next_token(); // eat extern.
            self.parse_prototype()
        }
    }
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

mod codegen {
    use std::collections::HashMap;

    use jit::insts::*;
    use jit::prelude::*;

    use ast;
    use errors::{ErrorKind, Result};

    pub struct CodeGenerator {
        pub context: Context,
        pub module: Module,
        pub builder: IRBuilder,
        pub named_values: HashMap<String, ValueRef>,
    }

    pub fn new(name: &str) -> CodeGenerator {
        let context = Context::new();
        let module = context.create_module(name);
        let builder = context.create_builder();

        CodeGenerator {
            context,
            module,
            builder,
            named_values: HashMap::new(),
        }
    }

    impl ast::Expr for ast::NumberExpr {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            Ok(gen.context.double_t().real(self.val).into())
        }
    }

    impl ast::Expr for ast::VariableExpr {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            gen.named_values.get(&self.name).map(|v| *v).ok_or_else(
                || {
                    ErrorKind::UnknownVariable(self.name.clone()).into()
                },
            )
        }
    }

    impl ast::Expr for ast::BinaryExpr {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            let f64_t = gen.context.double_t();

            let lhs = self.lhs.codegen(gen)?;
            let rhs = self.rhs.codegen(gen)?;

            Ok(
                match self.op {
                    ast::BinOp::Add => add!(lhs, rhs; "addtmp").emit_to(&gen.builder),
                    ast::BinOp::Sub => sub!(lhs, rhs; "subtmp").emit_to(&gen.builder),
                    ast::BinOp::Mul => mul!(lhs, rhs; "multmp").emit_to(&gen.builder),
                    ast::BinOp::LessThen => {
                        uitofp!(fcmp!(ult lhs, rhs; "cmptmp"), f64_t; "booltmp")
                            .emit_to(&gen.builder)
                    }
                }.into(),
            )
        }
    }

    impl ast::Expr for ast::CallExpr {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            if let Some(func) = gen.module.get_function(&self.callee) {
                if self.args.len() != func.param_count() {
                    bail!(ErrorKind::IncorrectArguments(
                        self.args.len(),
                        func.param_count(),
                    ))
                }

                let mut args = vec![];

                for arg in &self.args {
                    args.push(arg.codegen(gen)?);
                }

                Ok(call(func, args, "calltmp").emit_to(&gen.builder).into())
            } else {
                bail!(ErrorKind::UnknownFunction(self.callee.clone()))
            }
        }
    }

    impl ast::Expr for ast::Prototype {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            let f64_t = gen.context.double_t();
            let doubles = vec![f64_t; self.args.len()];
            let func_t = FunctionType::new(f64_t, &doubles, false);
            let func = gen.module.add_function(&self.name, func_t);

            for (name, arg) in self.args.iter().zip(func.params()) {
                arg.set_name(name);
            }

            Ok(func.into())
        }
    }

    impl ast::Expr for ast::Function {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            if let Some(func) = gen.module.get_function(&self.proto.name) {
                func.delete();
            }

            let func: Function = self.proto.codegen(gen)?.into();

            // Create a new basic block to start insertion into.
            let bb = func.append_basic_block_in_context("entry", &gen.context);
            gen.builder.position_at_end(bb);

            // Record the function arguments in the NamedValues map.
            gen.named_values = func.params()
                .map(|arg| (String::from(arg.name().unwrap()), arg))
                .collect::<HashMap<String, ValueRef>>();

            trace!("{} params: {:?}", gen.named_values.len(), gen.named_values);

            let ret_val = self.body.codegen(gen)?;

            ret!(ret_val).emit_to(&gen.builder);

            //func.verify()?;

            Ok(func.into())
        }
    }
}

use jit::prelude::*;

use ast::Expr;
use codegen::CodeGenerator;
use errors::Result;
use lexer::Token;

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

impl<I> parser::Parser<I>
where
    I: Iterator<Item = char>,
{
    fn handle_definition(&mut self, gen: &mut CodeGenerator) -> Result<ValueRef> {
        self.parse_definition().and_then(|func| {
            println!("parsed a function difinition: {:?}", func);

            let ir = func.codegen(gen)?;

            println!("read function definition:\n{}", ir);

            Ok(ir)
        })
    }

    fn handle_extern(&mut self, gen: &mut CodeGenerator) -> Result<ValueRef> {
        self.parse_extern().and_then(|proto| {
            println!("parsed a extern prototype: {:?}", proto);

            let ir = proto.codegen(gen)?;

            println!("read extern:\n{}", ir);

            Ok(ir)
        })
    }

    fn handle_top_level_expression(&mut self, gen: &mut CodeGenerator) -> Result<ValueRef> {
        self.parse_top_level_expr().and_then(|func| {
            println!("parsed a top-level expr: {:?}", func);

            let ir = func.codegen(gen)?;

            println!("read top-level expression:\n{}", ir);

            Ok(ir)
        })
    }

    fn handle_top(&mut self, gen: &mut CodeGenerator) -> Result<Parsed> {
        // top ::= definition | external | expression | ';'
        match self.next_token().clone() {
            Token::Def => Ok(Parsed::Code(self.handle_definition(gen)?)),
            Token::Extern => Ok(Parsed::Code(self.handle_extern(gen)?)),
            Token::Eof => Ok(Parsed::ToEnd),
            token @ Token::Character(';') |
            token @ Token::Comment(_) => Ok(Parsed::Skipped(token)),
            _ => Ok(Parsed::Code(self.handle_top_level_expression(gen)?)),
        }
    }
}

enum Parsed {
    ToEnd,
    Code(ValueRef),
    Skipped(Token),
}

struct Lines<'a> {
    rl: rustyline::Editor<()>,
    prompt: (&'a str, &'a str),
}

impl<'a> Lines<'a> {
    pub fn new() -> Self {
        Lines {
            rl: rustyline::Editor::<()>::new(),
            prompt: ("ready> ", "... "),
        }
    }

    fn read_line(&mut self, first: bool) -> rustyline::Result<String> {
        self.rl.readline(if first {
            &self.prompt.0
        } else {
            &self.prompt.1
        })
    }
}

impl<'a> Iterator for Lines<'a> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.read_line(true) {
                Ok(line) => {
                    if line.as_str().trim().ends_with(";") {
                        return Some(line);
                    }

                    let mut lines: Vec<String> = vec![line.to_owned()];

                    loop {
                        match self.read_line(false) {
                            Ok(line) => {
                                let finished = line.as_str().trim().ends_with(";");

                                lines.push(line);

                                if finished {
                                    return Some(lines.join("\n"));
                                }
                            }
                            Err(rustyline::error::ReadlineError::Interrupted) |
                            Err(rustyline::error::ReadlineError::Eof) => return None,
                            Err(err) => {
                                error!("fail to read line, {}", err);

                                return None;
                            }
                        }
                    }
                }
                Err(rustyline::error::ReadlineError::Interrupted) |
                Err(rustyline::error::ReadlineError::Eof) => return None,
                Err(err) => {
                    error!("fail to read line, {}", err);

                    return None;
                }
            }
        }
    }
}

fn main() {
    pretty_env_logger::init().unwrap();

    // Make the module, which holds all the code.
    let mut gen = codegen::new("my cool jit");

    for code in Lines::new() {
        debug!("parsing code: {}", code);

        let mut parser = parser::new(code.chars());

        // Run the main "interpreter loop" now.
        loop {
            match parser.handle_top(&mut gen) {
                Ok(Parsed::ToEnd) => {
                    break;
                }
                Ok(Parsed::Skipped(token)) => trace!("skipped {:?}", token),
                Ok(_) => {}
                Err(err) => {
                    let token = parser.next_token();

                    println!("skip token {:?} for error recovery, {}", token, err);
                }
            }
        }
    }

    // Print out all of the generated code.
    println!("module:\n{}", gen.module);
}
