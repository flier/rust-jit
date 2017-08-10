#[macro_use]
extern crate log;
#[macro_use]
extern crate error_chain;
extern crate rustyline;

mod errors {
    use lexer::Token;

    error_chain!{
        foreign_links {

        }

        errors {
            UnexpectedToken(msg: String, token: Token) {
                description("unexpected token")
                display("{}, but got unexpected token: '{:?}'", msg, token)
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
    }

    /// gettok - Return the next token from standard input.
    pub fn gettok<I: Iterator<Item = char>>(iter: &mut I) -> Token {
        // Skip any whitespace.
        let mut iter = iter.skip_while(|c| c.is_whitespace()).peekable();

        if let Some(&c) = iter.peek() {
            if c.is_alphabetic() {
                // identifier: [a-zA-Z][a-zA-Z0-9]*
                let identifierStr: String = iter.take_while(|c| c.is_alphanumeric()).collect();

                match identifierStr.as_str() {
                    "def" => Token::Def,
                    "extern" => Token::Extern,
                    _ => Token::Identifier(identifierStr),
                }
            } else if c.is_digit(10) || c == '.' {
                // number: [0-9.]+
                let numberStr: String = iter.take_while(|&c| c.is_digit(10) || c == '.').collect();

                Token::Number(numberStr.parse().unwrap())
            } else if c == '#' {
                // Comment until end of line.
                let mut iter = iter.skip_while(|&c| c != '\n' && c != '\r');

                gettok(&mut iter)
            } else {
                // Otherwise, just return the character as its ascii value.
                Token::Character(iter.next().unwrap())
            }
        } else {
            // Check for end of file.  Don't eat the EOF.
            Token::Eof
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

    /// Number - Expression class for numeric literals like "1.0".
    #[derive(Debug)]
    pub struct Number {
        pub val: f64,
    }

    impl Expr for Number {}

    /// Variable - Expression class for referencing a variable, like "a".
    #[derive(Debug)]
    pub struct Variable {
        pub name: String,
    }

    impl Expr for Variable {}

    /// Binary - Expression class for a binary operator.
    #[derive(Debug)]
    pub struct Binary {
        pub op: BinOp,
        pub lhs: Box<Expr>,
        pub rhs: Box<Expr>,
    }

    impl Expr for Binary {}

    /// Call - Expression class for function calls.
    #[derive(Debug)]
    pub struct Call {
        pub callee: String,
        pub args: Vec<Box<Expr>>,
    }

    impl Expr for Call {}

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
    use lexer::{self, Token};

    pub struct Parser<I>
    where
        I: Iterator<Item = char>,
    {
        iter: I,
        /// the current token the parser is looking at.
        cur_token: Token,
    }

    pub fn new<I: Iterator<Item = char>>(iter: I) -> Parser<I> {
        Parser {
            iter: iter,
            cur_token: Token::Eof,
        }
    }

    impl<I> Parser<I>
    where
        I: Iterator<Item = char>,
    {
        /// reads another token from the lexer and updates `cur_token` with its results.
        pub fn next_token(&mut self) -> &Token {
            self.cur_token = lexer::gettok(&mut self.iter);

            trace!("current token: {:?}", self.cur_token);

            &self.cur_token
        }

        /// Get the precedence of the pending binary operator token.
        fn get_tok_precedence(&self) -> Result<i32> {
            if let Some(op) = self.cur_token.as_bin_op() {
                Ok(op as i32)
            } else {
                bail!(ErrorKind::UnexpectedToken(
                    "Expected `<`, `+`, `-` or `*`".into(),
                    self.cur_token.clone(),
                ));
            }
        }

        /// numberexpr ::= number
        fn parse_number_expr(&mut self) -> Result<Box<ast::Expr>> {
            match self.cur_token {
                Token::Number(val) => Ok(Box::new(ast::Number { val })),
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
                    let expr = self.parse_expression();

                    match *self.next_token() {
                        Token::Character(')') => expr,
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

            match *self.next_token() {
                Token::Character('(') => {
                    let mut args = vec![];

                    loop {
                        args.push(self.parse_expression()?);

                        match *self.next_token() {
                            Token::Character(')') => break,
                            Token::Character(',') => {}
                            ref token => {
                                bail!(ErrorKind::UnexpectedToken(
                                    "Expected `)` or `,` in argument list".into(),
                                    token.clone(),
                                ));
                            }
                        }
                    }

                    Ok(Box::new(ast::Call { callee: name, args }))
                }
                _ => Ok(Box::new(ast::Variable { name: name.clone() })),
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
                let tok_prec = self.get_tok_precedence()?;

                // If this is a binop that binds at least as tightly as the current binop,
                // consume it, otherwise we are done.
                if tok_prec < expr_prec {
                    return Ok(lhs);
                }

                // Okay, we know this is a binop.
                let op = self.cur_token.as_bin_op().unwrap();

                // Okay, we know this is a binop.
                let mut rhs = self.parse_primary()?;

                // If BinOp binds less tightly with RHS than the operator after RHS,
                // let the pending operator take RHS as its LHS.
                let next_prec = self.get_tok_precedence()?;

                if tok_prec < next_prec {
                    rhs = self.parse_bin_op_rhs(tok_prec + 1, rhs)?;
                }

                lhs = Box::new(ast::Binary { op, lhs, rhs })
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

            match *self.next_token() {
                Token::Character('(') => {}
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
                Token::Character(')') => {}
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
                Token::Def => {}
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
            self.parse_prototype()
        }
    }
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

use lexer::Token;

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

fn main() {
    let mut rl = rustyline::Editor::<()>::new();

    // Run the main "interpreter loop" now.
    loop {
        match rl.readline("ready> ") {
            Ok(line) => {
                let mut parser = parser::new(line.chars());

                loop {
                    /// top ::= definition | external | expression | ';'
                    match *parser.next_token() {
                        Token::Def => parser.handle_definition(),
                        Token::Extern => parser.handle_extern(),
                        Token::Eof => break,
                        Token::Character(';') => {
                            trace!("ignore top-level semicolons.")
                        }
                        _ => parser.handle_top_level_expression(),
                    }
                }
            }
            Err(rustyline::error::ReadlineError::Interrupted) |
            Err(rustyline::error::ReadlineError::Eof) => break,
            Err(err) => {
                error!("fail to read line, {}", err);

                break;
            }
        }
    }
}