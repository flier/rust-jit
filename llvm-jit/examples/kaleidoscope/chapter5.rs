#[macro_use]
extern crate log;
extern crate pretty_env_logger;
#[macro_use]
extern crate error_chain;
extern crate rustyline;
extern crate libc;
extern crate llvm_sys as llvm;
#[macro_use]
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

        // control
        If,
        Then,
        Else,
        For,
        In,

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
                        "if" => Token::If,
                        "then" => Token::Then,
                        "else" => Token::Else,
                        "for" => Token::For,
                        "in" => Token::In,
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

    /// IfExpr - Expression class for if/then/else.
    ///
    /// ```no_run
    /// def fib(x)
    ///     if x < 3 then
    ///         1
    ///     else
    ///         fib(x-1)+fib(x-2);
    /// ```
    #[derive(Debug)]
    pub struct IfExpr {
        pub cond: Box<Expr>,
        pub then: Box<Expr>,
        pub or_else: Box<Expr>,
    }

    /// ForExprAST - Expression class for for/in.
    ///
    /// ```no_run
    /// extern putchard(char);
    /// def printstar(n)
    ///     for i = 1, i < n, 1.0 in
    ///         putchard(42);  # ascii 42 = '*'
    /// ```
    #[derive(Debug)]
    pub struct ForExpr {
        pub var_name: String,
        pub start: Box<Expr>,
        pub end: Box<Expr>,
        pub step: Option<Box<Expr>>,
        pub body: Box<Expr>,
    }

    /// Prototype - This class represents the "prototype" for a function,
    /// which captures its name, and its argument names (thus implicitly the number
    /// of arguments the function takes).
    #[derive(Clone, Debug)]
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

    pub const ANNO_EXPR: &str = "__anon_expr";

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

    macro_rules! match_token {
        ($self_:ident, $token:pat => $code:block, $msg:expr) => {
            match $self_.cur_token {
                $token => $code,
                ref token => bail!(ErrorKind::UnexpectedToken($msg.into(), token.clone()))
            }
        }
    }

    macro_rules! eat_token {
        ($self_:ident, $token:pat => $code:block, $msg:expr) => {
            match_token!($self_, $token => {
                $self_.next_token();

                $code
            }, $msg)
        };
        ($self_:ident, $token:pat, $msg:expr) => {
            match_token!($self_, $token => {
                $self_.next_token();
            }, $msg)
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
            eat_token!(self, Token::Number(val) => {
                Ok(Box::new(ast::NumberExpr { val }))
            }, "Expected `number`")
        }

        /// parenexpr ::= '(' expression ')'
        fn parse_paren_expr(&mut self) -> Result<Box<ast::Expr>> {
            eat_token!(self, Token::Character('(') => {
                let expr = self.parse_expression();

                eat_token!(self, Token::Character(')'), "Expected `)`");

                expr
            }, "Expected `(`")
        }

        /// identifierexpr
        ///   ::= identifier
        ///   ::= identifier '(' expression* ')'
        fn parse_identifier_expr(&mut self) -> Result<Box<ast::Expr>> {
            let name = match_token!(self, Token::Identifier(ref name) => {
                name.clone()
            }, "Expected `identifier`");

            self.next_token(); // eat identifier.

            match self.cur_token {
                Token::Character('(') => {
                    self.next_token(); // eat (

                    let mut args = vec![];

                    match self.cur_token {
                        Token::Character(')') => {
                            self.next_token(); // Eat the ')'.
                        }
                        _ => {
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
                        }
                    }

                    Ok(Box::new(ast::CallExpr { callee: name, args }))
                }
                _ => Ok(Box::new(ast::VariableExpr { name: name.clone() })),
            }
        }

        /// ifexpr ::= 'if' expression 'then' expression 'else' expression
        fn parse_if_expr(&mut self) -> Result<Box<ast::Expr>> {
            eat_token!(self, Token::If, "Expected `if`");

            let cond = self.parse_expression()?;

            eat_token!(self, Token::Then, "Expected `then`");

            let then = self.parse_expression()?;

            eat_token!(self, Token::Else, "Expected `else`");

            let or_else = self.parse_expression()?;

            Ok(Box::new(ast::IfExpr {
                cond,
                then,
                or_else,
            }))
        }

        /// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
        fn parse_for_expr(&mut self) -> Result<Box<ast::Expr>> {
            eat_token!(self, Token::For, "Expected `for`");

            let var_name = match_token!(self, Token::Identifier(ref name) => {
                name.clone()
            }, "Expected `identifier` after `for`");

            self.next_token(); // eat the identifier.

            eat_token!(self, Token::Character('='), "Expected `=` after `for`");

            let start = self.parse_expression()?;

            eat_token!(self, Token::Character(','), "Expected `,` after `for`");

            let end = self.parse_expression()?;

            // The step value is optional.
            let step = match self.cur_token {
                Token::Character(',') => {
                    self.next_token(); // eat the `,`.

                    Some(self.parse_expression()?)
                }
                _ => None,
            };

            eat_token!(self, Token::In, "Expected `in` after `for`");

            let body = self.parse_expression()?;

            Ok(Box::new(ast::ForExpr {
                var_name,
                start,
                end,
                step,
                body,
            }))
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
                Token::If => self.parse_if_expr(),
                Token::For => self.parse_for_expr(),
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
            let name = match_token!(self, Token::Identifier(ref name) => {
                name.clone()
            }, "Expected function name in prototype");

            self.next_token(); // Eat the `id`

            match_token!(self, Token::Character('(') => {
                // ignore it
            }, "Expected `(` in prototype");

            let mut args = vec![];

            while let &Token::Identifier(ref name) = self.next_token() {
                args.push(name.clone());
            }

            eat_token!(self, Token::Character(')'), "Expected `)` in prototype");

            Ok(ast::Prototype { name, args })
        }

        /// definition ::= 'def' prototype expression
        pub fn parse_definition(&mut self) -> Result<ast::Function> {
            eat_token!(self, Token::Def, "Expected `def` in definition");

            let proto = self.parse_prototype()?;
            let body = self.parse_expression()?;

            Ok(ast::Function { proto, body })
        }

        /// toplevelexpr ::= expression
        pub fn parse_top_level_expr(&mut self) -> Result<ast::Function> {
            let expr = self.parse_expression()?;

            Ok(ast::Function {
                proto: ast::Prototype {
                    name: ANNO_EXPR.into(),
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
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use jit;
    use jit::insts::*;
    use jit::prelude::*;

    use ast::{self, Expr};
    use errors::{ErrorKind, Result};

    pub struct CodeGenerator<'a> {
        pub context: &'a Context,
        pub module: Module,
        pub builder: IRBuilder,
        pub passmgr: jit::FunctionPassManager,
        pub named_values: HashMap<String, ValueRef>,
        pub protos: Rc<RefCell<HashMap<String, ast::Prototype>>>,
    }

    impl<'a> CodeGenerator<'a> {
        fn create_pass_manager(module: &Module) -> jit::FunctionPassManager {
            // Create a new pass manager attached to it.
            let passmgr = jit::FunctionPassManager::for_module(&module);

            // Do simple "peephole" optimizations and bit-twiddling optzns.
            passmgr.add(jit::Pass::InstructionCombining);
            // Reassociate expressions.
            passmgr.add(jit::Pass::Reassociate);
            // Eliminate Common SubExpressions.
            passmgr.add(jit::Pass::GVN);
            // Simplify the control flow graph (deleting unreachable blocks, etc).
            passmgr.add(jit::Pass::CFGSimplification);

            passmgr.init();

            passmgr
        }

        fn get_function(&mut self, name: &str) -> Option<Function> {
            // First, check for an existing function from a previous 'extern' declaration.
            self.module.get_function(name).or_else(|| {
                // If not, check whether we can codegen the declaration from some existing prototype.
                let proto = self.protos.borrow().get(name).cloned();

                proto.map(
                    |proto| {
                        trace!("found prototype `{}`", name);

                        proto.codegen(self).unwrap().into()
                    }
                )
            })
        }
    }

    pub fn new<'a>(
        context: &'a Context,
        name: &str,
        protos: Rc<RefCell<HashMap<String, ast::Prototype>>>,
    ) -> CodeGenerator<'a> {
        // Open a new module.
        let module = context.create_module(name);
        let passmgr = CodeGenerator::create_pass_manager(&module);

        CodeGenerator {
            context,
            module,
            builder: context.create_builder(),
            passmgr,
            named_values: HashMap::new(),
            protos,
        }
    }

    impl ast::Expr for ast::NumberExpr {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            Ok(gen.context.double_t().real(self.val).into())
        }
    }

    impl ast::Expr for ast::VariableExpr {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            // Look this variable up in the function.
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
                    ast::BinOp::Add => fadd!(lhs, rhs; "addtmp").emit_to(&gen.builder),
                    ast::BinOp::Sub => fsub!(lhs, rhs; "subtmp").emit_to(&gen.builder),
                    ast::BinOp::Mul => fmul!(lhs, rhs; "multmp").emit_to(&gen.builder),
                    ast::BinOp::LessThen => {
                        let lhs = fcmp!(ULT lhs, rhs; "cmptmp");
                        // Convert bool 0/1 to double 0.0 or 1.0
                        uitofp!(lhs, f64_t; "booltmp").emit_to(&gen.builder)
                    }
                }.into(),
            )
        }
    }

    impl ast::Expr for ast::CallExpr {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            // Look up the name in the global module table.
            if let Some(func) = gen.get_function(&self.callee) {
                // If argument mismatch error.
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

    impl ast::Expr for ast::IfExpr {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            let f64_t = gen.context.double_t();

            // Convert condition to a bool by comparing non-equal to 0.0.
            let cond = fcmp!(ONE self.cond.codegen(gen)?, f64_t.real(0.0));

            let func = gen.builder.insert_block().unwrap().parent();

            // Create blocks for the then and else cases.
            // Insert the 'then' block at the end of the function.
            let then_bb = func.append_basic_block_in_context("then", gen.context);
            let else_bb = func.append_basic_block_in_context("else", gen.context);
            let merge_bb = func.append_basic_block_in_context("ifcont", gen.context);

            gen.builder <<= br!(cond => then_bb, _ => else_bb);

            // Emit then value.
            gen.builder.position_at_end(then_bb);

            let then = self.then.codegen(gen)?;

            gen.builder <<= br!(merge_bb);

            // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
            let then_bb = gen.builder.insert_block().unwrap();

            // Emit else block.
            gen.builder.position_at_end(else_bb);

            let or_else = self.or_else.codegen(gen)?;

            gen.builder <<= br!(merge_bb);

            // codegen of 'Else' can change the current block, update ElseBB for the PHI.
            let else_bb = gen.builder.insert_block().unwrap();

            // Emit merge block.
            gen.builder.position_at_end(merge_bb);

            let pn = phi!(f64_t, then => then_bb, or_else => else_bb; "iftmp")
                .emit_to(&gen.builder);

            Ok(pn.into())
        }
    }

    impl ast::Expr for ast::ForExpr {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            let f64_t = gen.context.double_t();

            // Emit the start code first, without 'variable' in scope.
            let start = self.start.codegen(gen)?;

            // Make the new basic block for the loop header, inserting after current block.
            let preheader_bb = gen.builder.insert_block().unwrap();
            let func = preheader_bb.parent();
            let loop_bb = func.append_basic_block_in_context("loop", gen.context);

            // Insert an explicit fall through from the current block to the LoopBB.
            gen.builder <<= br!(loop_bb);

            // Start insertion in LoopBB.
            gen.builder.position_at_end(loop_bb);

            // Start the PHI node with an entry for Start.
            let var = phi!(f64_t, start => preheader_bb; self.var_name.clone())
                .emit_to(&gen.builder);

            // Within the loop, the variable is defined equal to the PHI node.
            // If it shadows an existing variable, we have to restore it, so save it now.
            let old_value = gen.named_values.insert(self.var_name.clone(), var.into());

            // Emit the body of the loop.
            // This, like any other expr, can change the current BB.
            // Note that we ignore the value computed by the body, but don't allow an error.
            self.body.codegen(gen)?;

            // Emit the step value.
            let step_val = if let Some(ref step) = self.step {
                step.codegen(gen)?
            } else {
                f64_t.real(1.0).into()
            };

            let next_val = fadd!(var, step_val; "nextvar").emit_to(&gen.builder);

            // Compute the end condition.
            let end_val = self.end.codegen(gen)?;

            // Convert condition to a bool by comparing non-equal to 0.0.
            let end_cond = fcmp!(ONE end_val, f64_t.real(0.0); "loopcond");

            // Create the "after loop" block and insert it.
            let loop_end_bb = gen.builder.insert_block().unwrap();
            let after_bb = func.append_basic_block_in_context("afterloop", gen.context);

            // Insert the conditional branch into the end of LoopEndBB.
            gen.builder <<= br!(end_cond => loop_bb, _ => after_bb);

            // Any new code will be inserted in AfterBB.
            gen.builder.position_at_end(after_bb);

            // Add a new entry to the PHI node for the backedge.
            var.add_incomings(&[(next_val.into(), loop_end_bb)]);

            // Restore the unshadowed variable.
            if let Some(value) = old_value {
                gen.named_values.insert(self.var_name.clone(), value);
            } else {
                gen.named_values.remove(&self.var_name);
            }

            // for expr always returns 0.0.
            Ok(f64_t.null().into())
        }
    }

    impl ast::Expr for ast::Prototype {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            // Make the function type:  double(double,double) etc.
            let f64_t = gen.context.double_t();
            let doubles = vec![f64_t; self.args.len()];
            let func_t = FunctionType::new(f64_t, &doubles, false);
            let func = gen.module.add_function(&self.name, func_t);

            // Set names for all arguments.
            for (name, arg) in self.args.iter().zip(func.params()) {
                arg.set_name(name);
            }

            Ok(func.into())
        }
    }

    impl ast::Expr for ast::Function {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            gen.protos.borrow_mut().insert(
                self.proto.name.clone(),
                self.proto.clone(),
            );

            let func = if let Some(func) = gen.get_function(&self.proto.name) {
                func
            } else {
                bail!("not found `{}` function", self.proto.name)
            };

            // Create a new basic block to start insertion into.
            let bb = func.append_basic_block_in_context("entry", &gen.context);
            gen.builder.position_at_end(bb);

            // Record the function arguments in the NamedValues map.
            gen.named_values = func.params()
                .map(|arg| (String::from(arg.name().unwrap()), arg))
                .collect::<HashMap<String, ValueRef>>();

            trace!("{} params: {:?}", gen.named_values.len(), gen.named_values);

            let ret_val = self.body.codegen(gen)?;

            // Finish off the function.
            gen.builder <<= ret!(ret_val);

            // Validate the generated code, checking for consistency.
            if func.verify().is_err() {
                gen.module.verify()?;
            }

            gen.passmgr.run(&func);

            Ok(func.into())
        }
    }
}

use std::cell::RefCell;
use std::char;
use std::collections::HashMap;
use std::ffi::CStr;
use std::mem;
use std::rc::Rc;

use libc::c_void;

use jit::prelude::*;
use jit::target::*;

use ast::Expr;
use codegen::CodeGenerator;
use errors::Result;
use lexer::Token;

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

impl<I> parser::Parser<I>
where
    I: Iterator<Item = char>,
{
    fn handle_definition(
        &mut self,
        mut gen: CodeGenerator,
        engine: &mut KaleidoscopeJIT,
    ) -> Result<ValueRef> {
        self.parse_definition().and_then(move |func| {
            println!("parsed a function difinition: {:?}", func);

            let ir = func.codegen(&mut gen)?;

            println!("read function definition:\n{}", ir);

            engine.add_module(gen.module);

            Ok(ir)
        })
    }

    fn handle_extern(
        &mut self,
        mut gen: CodeGenerator,
        _engine: &mut KaleidoscopeJIT,
    ) -> Result<ValueRef> {
        self.parse_extern().and_then(|proto| {
            println!("parsed a extern prototype: {:?}", proto);

            let ir = proto.codegen(&mut gen)?;

            println!("read extern:\n{}", ir);

            gen.protos.borrow_mut().insert(proto.name.clone(), proto);

            Ok(ir)
        })
    }

    fn handle_top_level_expression(
        &mut self,
        mut gen: CodeGenerator,
        engine: &mut KaleidoscopeJIT,
    ) -> Result<ValueRef> {
        self.parse_top_level_expr().and_then(move |func| {
            println!("parsed a top-level expr: {:?}", func);

            let ir = func.codegen(&mut gen)?;

            println!("read top-level expression:\n{}", ir);

            // JIT the module containing the anonymous expression,
            // keeping a handle so we can free it later.
            let handle = engine.add_module(gen.module);

            // Search the JIT for the __anon_expr symbol.
            let addr = engine.find_symbol(parser::ANNO_EXPR).unwrap();

            // Get the symbol's address and cast it to the right type
            // (takes no arguments, returns a double) so we can call it as a native function.
            let fp: extern "C" fn() -> f64 = unsafe { mem::transmute(addr) };

            println!("Evaluated to: {}", fp());

            // Delete the anonymous expression module from the JIT.
            engine.remove_module(handle);

            Ok(ir)
        })
    }

    fn handle_top(&mut self, gen: CodeGenerator, engine: &mut KaleidoscopeJIT) -> Result<Parsed> {
        // top ::= definition | external | expression | ';'
        match self.next_token().clone() {
            Token::Def => Ok(Parsed::Code(self.handle_definition(gen, engine)?)),
            Token::Extern => Ok(Parsed::Code(self.handle_extern(gen, engine)?)),
            Token::Eof => Ok(Parsed::ToEnd),
            token @ Token::Character(';') |
            token @ Token::Comment(_) => Ok(Parsed::Skipped(token)),
            _ => Ok(Parsed::Code(self.handle_top_level_expression(gen, engine)?)),
        }
    }
}

struct KaleidoscopeJIT {
    pub engine: jit::JITStack,
    pub modules: Vec<jit::ModuleHandle>,
    pub protos: Rc<RefCell<HashMap<String, ast::Prototype>>>,
}

impl KaleidoscopeJIT {
    pub fn new(target_machine: &TargetMachine) -> Result<KaleidoscopeJIT> {
        let engine = jit::JITStack::new(target_machine);

        Ok(KaleidoscopeJIT {
            engine,
            modules: Vec::new(),
            protos: Rc::new(RefCell::new(HashMap::new())),
        })
    }

    pub fn add_module(&mut self, module: Module) -> jit::ModuleHandle {
        let ctx = self as *mut KaleidoscopeJIT;
        let handle = self.engine.add_eagerly_compiled_ir(
            module,
            Some(symbol_resolver_callback),
            Some(unsafe { &mut *ctx }),
        );

        self.modules.push(handle);

        handle
    }

    pub fn remove_module(&mut self, handle: jit::ModuleHandle) -> bool {
        self.modules
            .iter()
            .position(|&h| h == handle)
            .map(|pos| self.modules.remove(pos))
            .map(|handle| self.engine.remove_module(handle))
            .is_some()
    }

    pub fn add_symbol<S: AsRef<str>>(&self, symbol: S, addr: Option<ExternFn>) {
        let name = self.engine.get_mangled_symbol(symbol);
        let addr: *const c_void = unsafe { mem::transmute(addr) };

        jit::Symbols::add_symbol(name, addr)
    }

    pub fn find_symbol<S: AsRef<str>>(&self, symbol: S) -> Option<jit::TargetAddress> {
        let symbol = symbol.as_ref();

        trace!("finding symbol `{}`", symbol);

        self.engine
            .get_symbol_address(symbol)
            .map(|addr| {
                trace!("got symbol `{}` from JIT engine @ {:?}", symbol, addr);

                addr
            })
            .or_else(|| {
                jit::Symbols::search_for_address(&symbol).map(|p: *const c_void| {
                    trace!("got symbol `{}` @ {:?}", symbol, p);

                    unsafe { mem::transmute(p) }
                })
            })
    }
}

pub type ExternFn = extern "C" fn(f64) -> f64;

extern "C" fn symbol_resolver_callback(
    symbol: *const libc::c_char,
    ctx: *mut libc::c_void,
) -> jit::TargetAddress {
    let jit: &KaleidoscopeJIT = unsafe { &*(ctx as *const KaleidoscopeJIT) };
    let symbol = unsafe { CStr::from_ptr(symbol) }.to_string_lossy();

    trace!("resolving symbol `{}`", symbol);

    jit.find_symbol(symbol).unwrap_or(0)
}

enum Parsed {
    ToEnd,
    Code(ValueRef),
    Skipped(Token),
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

/// putchard - putchar that takes a double and returns 0.
extern "C" fn putchard(x: f64) -> f64 {
    print!("{}", char::from_u32(x as u32).unwrap_or_default());
    0.0
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    0.0
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

fn main() {
    pretty_env_logger::init().unwrap();

    NativeTarget::init().unwrap();
    NativeAsmParser::init().unwrap();
    NativeAsmPrinter::init().unwrap();

    jit::MCJITCompiler::link_in();

    let context = Context::new();

    let target_machine = TargetMachine::default();
    let mut engine = KaleidoscopeJIT::new(&target_machine).expect("create JIT compiler");

    engine.add_symbol("putchard", Some(putchard));
    engine.add_symbol("printd", Some(printd));

    let mut parser = parser::new(lines::Lines::new());

    // Run the main "interpreter loop" now.
    loop {
        match parser.handle_top(
            codegen::new(&context, "my cool jit", engine.protos.clone()),
            &mut engine,
        ) {
            Ok(Parsed::ToEnd) => {
                break;
            }
            Ok(Parsed::Skipped(token)) => trace!("skipped {:?}", token),
            Ok(_) => {}
            Err(err) => {
                let token = parser.next_token();

                match *token {
                    Token::Eof => break,
                    _ => {
                        println!("skip token {:?} for error recovery, {}", token, err);
                    }
                }
            }
        }
    }
}
