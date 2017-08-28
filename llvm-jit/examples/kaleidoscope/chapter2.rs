#[macro_use]
extern crate log;
extern crate pretty_env_logger;
#[macro_use]
extern crate error_chain;
extern crate rustyline;
extern crate llvm_jit as jit;

mod errors;
mod lines;

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
    pub trait Expr: fmt::Debug {}

    /// NumberExpr - Expression class for numeric literals like "1.0".
    #[derive(Debug)]
    pub struct NumberExpr {
        pub val: f64,
    }

    impl Expr for NumberExpr {}

    /// VariableExpr - Expression class for referencing a variable, like "a".
    #[derive(Debug)]
    pub struct VariableExpr {
        pub name: String,
    }

    impl Expr for VariableExpr {}

    /// BinaryExpr - Expression class for a binary operator.
    #[derive(Debug)]
    pub struct BinaryExpr {
        pub op: BinOp,
        pub lhs: Box<Expr>,
        pub rhs: Box<Expr>,
    }

    impl Expr for BinaryExpr {}

    /// CallExpr - Expression class for function calls.
    #[derive(Debug)]
    pub struct CallExpr {
        pub callee: String,
        pub args: Vec<Box<Expr>>,
    }

    impl Expr for CallExpr {}

    /// Prototype - This class represents the "prototype" for a function,
    /// which captures its name, and its argument names (thus implicitly the number
    /// of arguments the function takes).
    #[derive(Debug)]
    pub struct Prototype {
        pub name: String,
        pub args: Vec<String>,
    }

    impl Expr for Prototype {}

    /// FunctionAST - This class represents a function definition itself.
    #[derive(Debug)]
    pub struct Function {
        pub proto: Prototype,
        pub body: Box<Expr>,
    }

    impl Expr for Function {}
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

use lexer::Token;

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

impl<I> parser::Parser<I>
where
    I: Iterator<Item = char>,
{
    fn handle_definition(&mut self) {
        match self.parse_definition() {
            Ok(func) => println!("parsed a function difinition: {:?}", func),
            Err(err) => {
                let token = self.next_token();

                warn!("skip token {:?} for error recovery, {}", token, err)
            }
        }
    }

    fn handle_extern(&mut self) {
        match self.parse_extern() {
            Ok(proto) => println!("parsed a extern prototype: {:?}", proto),
            Err(err) => {
                let token = self.next_token();

                warn!("skip token {:?} for error recovery, {}", token, err)
            }
        }
    }

    fn handle_top_level_expression(&mut self) {
        match self.parse_top_level_expr() {
            Ok(func) => println!("parsed a top-level expr: {:?}", func),
            Err(err) => {
                let token = self.next_token();

                warn!("skip token {:?} for error recovery, {}", token, err)
            }
        }
    }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

fn main() {
    pretty_env_logger::init().unwrap();

    let mut parser = parser::new(lines::Lines::new());

    loop {
        // top ::= definition | external | expression | ';'
        match parser.next_token().clone() {
            Token::Def => parser.handle_definition(),
            Token::Extern => parser.handle_extern(),
            Token::Eof => break,
            Token::Character(';') => trace!("ignore top-level semicolons."),
            Token::Comment(ref comment) => trace!("ignore comment: {}", comment),
            _ => parser.handle_top_level_expression(),
        }
    }
}
