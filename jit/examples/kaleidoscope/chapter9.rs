#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;

#[macro_use]
extern crate llvm_jit as jit;
extern crate llvm_sys as llvm;

mod lines;

//===----------------------------------------------------------------------===//
// errors
//===----------------------------------------------------------------------===//

mod errors {
    use crate::lexer::{Spanned, Token};

    #[derive(Fail, Debug)]
    pub enum ErrorKind {
        #[fail(display = "{}, but got unexpected token: {:?}", _0, _1)]
        UnexpectedToken(String, Option<Spanned<Token>>),

        #[fail(display = "unknown variable: {}", _0)]
        UnknownVariable(String),

        #[fail(display = "unknown function: {}", _0)]
        UnknownFunction(String),

        #[fail(display = "incorrect arguments, passed {}, expected {}", _0, _1)]
        IncorrectArguments(usize, usize),
    }

    pub type Result<T> = ::std::result::Result<T, ::failure::Error>;
}

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

mod lexer {
    use std::fmt;
    use std::ops::{Add, BitOr, BitOrAssign, Deref, Sub};

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

        // operators
        BinaryOp,
        UnaryOp,

        // primary
        Identifier(String),
        Number(f64),
        Character(char),
        Comment(String),

        // var definition
        Var,
    }

    pub struct Lexer<I: Iterator<Item = char>> {
        iter: Backable<Locatable<I>>,
    }

    impl<I: Iterator<Item = char>> Lexer<I> {
        pub fn new(iter: I) -> Self {
            Lexer {
                iter: Backable::new(Locatable::new(iter)),
            }
        }

        pub fn location(&self) -> LineColumn {
            match self.iter.front {
                Some(Some((_, loc))) => loc,
                _ => self.iter.iter.location(),
            }
        }
    }

    impl<I> Iterator for Lexer<I>
    where
        I: Iterator<Item = char>,
    {
        type Item = Spanned<Token>;

        fn next(&mut self) -> Option<Self::Item> {
            match self.gettok() {
                Spanned { item: Token::Eof, .. } => None,
                token => {
                    trace!("parsed token: {:?}", token);

                    Some(token)
                }
            }
        }
    }

    impl<I> Lexer<I>
    where
        I: Iterator<Item = char>,
    {
        /// gettok - Return the next token from standard input.
        pub fn gettok(&mut self) -> Spanned<Token> {
            // Skip any whitespace.
            let c = self.iter.by_ref().skip_while(|(c, _)| c.is_whitespace()).next();

            if let Some((c, start)) = c {
                if c.is_alphabetic() {
                    self.iter.step_back();

                    // identifier: [a-zA-Z][a-zA-Z0-9]*
                    let s: String = self
                        .iter
                        .by_ref()
                        .map(|(c, _)| c)
                        .take_while(|c| c.is_alphanumeric())
                        .collect();

                    let span = span(start, self.location() - 1);

                    self.iter.step_back();

                    match s.as_str() {
                        "def" => Token::Def,
                        "extern" => Token::Extern,
                        "if" => Token::If,
                        "then" => Token::Then,
                        "else" => Token::Else,
                        "for" => Token::For,
                        "in" => Token::In,
                        "binary" => Token::BinaryOp,
                        "unary" => Token::UnaryOp,
                        "var" => Token::Var,
                        _ => Token::Identifier(s),
                    }
                    .with_span(span)
                } else if c.is_digit(10) || c == '.' {
                    self.iter.step_back();

                    // number: [0-9.]+
                    let s: String = self
                        .iter
                        .by_ref()
                        .map(|(c, _)| c)
                        .take_while(|c| c.is_digit(10) || *c == '.')
                        .collect();

                    let span = span(start, self.location());

                    self.iter.step_back();

                    Token::Number(s.parse().unwrap()).with_span(span)
                } else if c == '#' {
                    // Comment until end of line.
                    let s: String = self
                        .iter
                        .by_ref()
                        .map(|(c, _)| c)
                        .take_while(|c| *c != '\n' && *c != '\r')
                        .collect();

                    let end = LineColumn {
                        line: self.location().line + 1,
                        column: 0,
                    };

                    Token::Comment(s.to_owned()).with_span(span(start, end))
                } else {
                    // Otherwise, just return the character as its ascii value.
                    Token::Character(c).with_span(span(start, self.location()))
                }
            } else {
                // Check for end of file.  Don't eat the EOF.
                Token::Eof.with_span(span(self.location(), self.location()))
            }
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
    }

    impl<I: Iterator> Backable<I>
    where
        I::Item: fmt::Debug + Copy,
    {
        pub fn step_back(&mut self) {
            self.front = self.back.take();
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
                let item = self.iter.next();

                self.back = Some(item.clone());

                item
            }
        }
    }

    struct Locatable<I> {
        iter: I,
        loc: LineColumn,
    }

    impl<I> Locatable<I> {
        pub fn new(iter: I) -> Self {
            Locatable {
                iter,
                loc: LineColumn { line: 1, column: 1 },
            }
        }

        pub fn location(&self) -> LineColumn {
            self.loc
        }
    }

    impl<I: Iterator<Item = char>> Iterator for Locatable<I> {
        type Item = (char, LineColumn);

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(item) = self.iter.next() {
                let loc = self.loc;

                match item {
                    '\n' => {
                        self.loc.line += 1;
                        self.loc.column = 1;
                    }
                    _ => {
                        self.loc.column += 1;
                    }
                }

                Some((item, loc))
            } else {
                None
            }
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Default, Ord, Eq)]
    pub struct LineColumn {
        pub line: usize,
        pub column: usize,
    }

    impl fmt::Display for LineColumn {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}:{}", self.line, self.column)
        }
    }

    impl Add<usize> for LineColumn {
        type Output = LineColumn;

        fn add(self, rhs: usize) -> Self::Output {
            LineColumn {
                line: self.line,
                column: self.column + rhs,
            }
        }
    }

    impl Sub<usize> for LineColumn {
        type Output = LineColumn;

        fn sub(self, rhs: usize) -> Self::Output {
            LineColumn {
                line: self.line,
                column: self.column - rhs,
            }
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq, Default)]
    pub struct Span {
        pub start: LineColumn,
        pub end: LineColumn,
    }

    pub fn span(start: LineColumn, end: LineColumn) -> Span {
        Span { start, end }
    }

    impl Span {
        pub fn join(&self, other: Span) -> Span {
            Span {
                start: self.start.min(other.start),
                end: self.end.max(other.end),
            }
        }
    }

    impl fmt::Display for Span {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "[{}..{})", self.start, self.end)
        }
    }

    impl BitOr for Span {
        type Output = Span;

        fn bitor(self, rhs: Span) -> Self::Output {
            self.join(rhs)
        }
    }

    impl BitOrAssign for Span {
        fn bitor_assign(&mut self, rhs: Span) {
            *self = self.join(rhs)
        }
    }

    #[derive(Clone, PartialEq)]
    pub struct Spanned<T> {
        pub item: T,
        pub span: Span,
    }

    impl<T> Deref for Spanned<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.item
        }
    }

    impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{:?} @ {}", self.item, self.span)
        }
    }

    pub trait Spannable
    where
        Self: Sized,
    {
        fn with_span(self, span: Span) -> Spanned<Self> {
            Spanned { item: self, span }
        }
    }

    impl Spannable for Token {}
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

mod ast {
    use std::fmt;

    use crate::lexer::{Span, Spannable, Spanned, Token};

    #[derive(Clone, Debug, PartialEq)]
    pub enum BinOp {
        Assignment,
        LessThen,
        Add,
        Sub,
        Mul,
        UserDefined(char),
    }

    impl fmt::Display for BinOp {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let op = match *self {
                BinOp::Assignment => '=',
                BinOp::LessThen => '<',
                BinOp::Add => '+',
                BinOp::Sub => '-',
                BinOp::Mul => '*',
                BinOp::UserDefined(op) => op,
            };

            write!(f, "{}", op)
        }
    }

    impl Token {
        pub fn as_bin_op(&self) -> Option<BinOp> {
            match *self {
                Token::Character('=') => Some(BinOp::Assignment),
                Token::Character('<') => Some(BinOp::LessThen),
                Token::Character('+') => Some(BinOp::Add),
                Token::Character('-') => Some(BinOp::Sub),
                Token::Character('*') => Some(BinOp::Mul),
                Token::Character(op) => Some(BinOp::UserDefined(op)),
                _ => None,
            }
        }
    }

    /// Expr - Base class for all expression nodes.
    #[derive(Clone, Debug, PartialEq)]
    pub enum Expr {
        Number(Spanned<Number>),
        Variable(Spanned<Variable>),
        Unary(Spanned<Unary>),
        Binary(Spanned<Binary>),
        Call(Spanned<Call>),
        If(Spanned<If>),
        For(Spanned<For>),
        Var(Spanned<Var>),
    }

    impl Expr {
        pub fn span(&self) -> Span {
            match self {
                Expr::Number(expr) => expr.span,
                Expr::Variable(expr) => expr.span,
                Expr::Unary(expr) => expr.span,
                Expr::Binary(expr) => expr.span,
                Expr::Call(expr) => expr.span,
                Expr::If(expr) => expr.span,
                Expr::For(expr) => expr.span,
                Expr::Var(expr) => expr.span,
            }
        }
    }

    /// Number - Expression class for numeric literals like "1.0".
    #[derive(Clone, Debug, PartialEq)]
    pub struct Number {
        pub val: f64,
    }

    /// Variable - Expression class for referencing a variable, like "a".
    #[derive(Clone, Debug, PartialEq)]
    pub struct Variable {
        pub name: String,
    }

    /// Unary - Expression class for a unary operator.
    #[derive(Clone, Debug, PartialEq)]
    pub struct Unary {
        pub opcode: char,
        pub operand: Box<Expr>,
    }

    impl Unary {
        pub fn function_name(&self) -> String {
            format!("unary{}", self.opcode)
        }
    }

    /// Binary - Expression class for a binary operator.
    #[derive(Clone, Debug, PartialEq)]
    pub struct Binary {
        pub op: BinOp,
        pub lhs: Box<Expr>,
        pub rhs: Box<Expr>,
    }

    impl Binary {
        pub fn function_name(&self) -> String {
            format!("binary{}", self.op)
        }
    }

    /// CallExpr - Expression class for function calls.
    #[derive(Clone, Debug, PartialEq)]
    pub struct Call {
        pub callee: String,
        pub args: Vec<Expr>,
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
    #[derive(Clone, Debug, PartialEq)]
    pub struct If {
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
    #[derive(Clone, Debug, PartialEq)]
    pub struct For {
        pub var_name: String,
        pub start: Box<Expr>,
        pub end: Box<Expr>,
        pub step: Option<Box<Expr>>,
        pub body: Box<Expr>,
    }

    /// VarExpr - Expression class for var/in
    #[derive(Clone, Debug, PartialEq)]
    pub struct Var {
        pub vars: Vec<(String, Option<Expr>)>,
        pub body: Box<Expr>,
    }

    /// Prototype - This class represents the "prototype" for a function,
    /// which captures its name, and its argument names (thus implicitly the number
    /// of arguments the function takes).
    #[derive(Clone, Debug, PartialEq)]
    pub struct Prototype {
        pub name: String,
        pub args: Vec<String>,
        pub is_operator: bool,
        /// Precedence if a binary op.
        pub precedence: Option<isize>,
    }

    /// FunctionAST - This class represents a function definition itself.
    #[derive(Clone, Debug, PartialEq)]
    pub struct Function {
        pub proto: Spanned<Prototype>,
        pub body: Expr,
    }

    impl Spannable for Number {}
    impl Spannable for Variable {}
    impl Spannable for Unary {}
    impl Spannable for Binary {}
    impl Spannable for Call {}
    impl Spannable for If {}
    impl Spannable for For {}
    impl Spannable for Var {}
    impl Spannable for Prototype {}
    impl Spannable for Function {}
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

mod parser {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use crate::ast;
    use crate::errors::{ErrorKind, Result};
    use crate::lexer::{span, Lexer, Spannable, Spanned, Token};

    pub struct Parser<I>
    where
        I: Iterator<Item = char>,
    {
        lexer: Lexer<I>,
        /// the current token the parser is looking at.
        cur_token: Option<Spanned<Token>>,
        pub binop_precedences: Rc<RefCell<HashMap<String, isize>>>,
    }

    pub fn new<I>(iter: I) -> Parser<I>
    where
        I: Iterator<Item = char>,
    {
        Parser {
            lexer: Lexer::new(iter),
            cur_token: None,
            binop_precedences: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    macro_rules! match_token {
        ($self_:ident $( , | $token:pat, $span:ident | => $code:block )* | => $default:block ) => {
            match $self_.cur_token.clone() {
                $( Some(Spanned { item: $token, span: $span }) => $code , )*
                _ => $default
            }
        };
        ($self_:ident $( , | $token:pat, $span:ident | => $code:block )* | $msg:expr) => {
            match $self_.cur_token.clone() {
                $( Some(Spanned { item: $token, span: $span }) => $code , )*
                token => bail!(ErrorKind::UnexpectedToken($msg.into(), token))
            }
        };
        ($self_:ident $( , | $token:pat, $span:ident | => $code:block )* ) => {
            match $self_.cur_token.clone() {
                $( Some(Spanned { item: $token, span: $span }) => $code , )*
            }
        };

        ($self_:ident, $( $token:pat => $code:block ),* | _ => $default:block ) => {
            match $self_.cur_token.clone() {
                $( Some(Spanned { item: $token, span: _ }) => $code ),*
                _ => $default
            }
        };
        ($self_:ident, $( $token:pat => $code:block ),* | $msg:expr) => {
            match $self_.cur_token.clone() {
                $( Some(Spanned { item: $token, span: _ }) => $code , )*
                token => bail!(ErrorKind::UnexpectedToken($msg.into(), token))
            }
        };
        ($self_:ident, $( $token:pat => $code:block ),*) => {
            match $self_.cur_token.clone() {
                $( Some(Spanned { item: $token, span: _ }) => $code ),*
            }
        }
    }

    macro_rules! eat_token {
        ($self_:ident $( , | $token:pat, $span:ident | => $code:block )* | { $default:block } ) => {
            match_token!($self_ $( , | $token, $span | => {
                $self_.next_token();

                $code
            } ),*, | { $default })
        };        ($self_:ident $( , | $token:pat, $span:ident | => $code:block )* | $msg:expr) => {
            match_token!($self_ $( , | $token, $span | => {
                $self_.next_token();

                $code
            } ),* | $msg)
        };

        ($self_:ident, $( $token:pat => $code:block ),* | $msg:expr) => {
            match_token!($self_, $( $token => {
                $self_.next_token();

                $code
            } ),* | $msg)
        };
        ($self_:ident, $( $token:pat ),* | $msg:expr) => {
            match_token!($self_, $( $token => {
                $self_.next_token()
            } ),* | $msg)
        }
    }

    impl<I> Parser<I>
    where
        I: Iterator<Item = char>,
    {
        /// reads another token from the lexer and updates `cur_token` with its results.
        pub fn next_token(&mut self) -> Spanned<Token> {
            self.cur_token = self.lexer.next();

            self.cur_token
                .clone()
                .unwrap_or_else(|| Token::Eof.with_span(span(self.lexer.location(), self.lexer.location())))
        }

        /// Get the precedence of the pending binary operator token.
        fn get_tok_precedence(&self) -> Option<i32> {
            self.cur_token.as_ref().and_then(|tok| {
                tok.as_bin_op().and_then(|op| match op {
                    ast::BinOp::Assignment => Some(2),
                    ast::BinOp::LessThen => Some(10),
                    ast::BinOp::Add => Some(20),
                    ast::BinOp::Sub => Some(20),
                    ast::BinOp::Mul => Some(40),
                    ast::BinOp::UserDefined(op) => self
                        .binop_precedences
                        .borrow()
                        .get(format!("binary{}", op).as_str())
                        .map(|&v| v as i32),
                })
            })
        }

        /// numberexpr ::= number
        fn parse_number_expr(&mut self) -> Result<ast::Expr> {
            trace!("parsing number expression, cur_token = {:?}", self.cur_token);

            eat_token!(self, |Token::Number(val), span| => {
                Ok(ast::Expr::Number(ast::Number{ val }.with_span(span)))
            } | "Expected `number`")
        }

        /// parenexpr ::= '(' expression ')'
        fn parse_paren_expr(&mut self) -> Result<ast::Expr> {
            trace!("parsing paren expression, cur_token = {:?}", self.cur_token);

            eat_token!(self, Token::Character('(') => {
                let expr = self.parse_expression();

                eat_token!(self, Token::Character(')') | "Expected `)`");

                expr
            } | "Expected `(`")
        }

        /// identifierexpr
        ///   ::= identifier
        ///   ::= identifier '(' expression* ')'
        fn parse_identifier_expr(&mut self) -> Result<ast::Expr> {
            trace!("parsing ident expression, cur_token = {:?}", self.cur_token);

            let (name, mut curr_span) = match_token!(self, |Token::Identifier(name), span| => {
                (name, span)
            } | "Expected `identifier`");

            self.next_token(); // eat identifier.

            match self.cur_token {
                Some(Spanned {
                    item: Token::Character('('),
                    span,
                }) => {
                    curr_span |= span;

                    self.next_token(); // Eat the '('.

                    let mut args = vec![];

                    match self.cur_token {
                        Some(Spanned {
                            item: Token::Character(')'),
                            span,
                        }) => {
                            curr_span |= span;

                            self.next_token(); // Eat the ')'.
                        }
                        _ => {
                            loop {
                                args.push(self.parse_expression()?);

                                match self.cur_token {
                                    Some(Spanned {
                                        item: Token::Character(')'),
                                        span,
                                    }) => {
                                        curr_span |= span;

                                        self.next_token(); // Eat the ')'.

                                        break;
                                    }
                                    Some(Spanned {
                                        item: Token::Character(','),
                                        span,
                                    }) => {
                                        curr_span |= span;

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

                    Ok(ast::Expr::Call(ast::Call { callee: name, args }.with_span(curr_span)))
                }
                _ => Ok(ast::Expr::Variable(ast::Variable { name }.with_span(curr_span))),
            }
        }

        /// ifexpr ::= 'if' expression 'then' expression 'else' expression
        fn parse_if_expr(&mut self) -> Result<ast::Expr> {
            trace!("parsing if expression, cur_token = {:?}", self.cur_token);

            let if_span = eat_token!(self, |Token::If, span| => { span } | "Expected `if`");

            let cond = Box::new(self.parse_expression()?);

            eat_token!(self, Token::Then | "Expected `then`");

            let then = Box::new(self.parse_expression()?);

            eat_token!(self, Token::Else | "Expected `else`");

            let or_else = Box::new(self.parse_expression()?);

            let span = if_span | or_else.span();

            Ok(ast::Expr::If(ast::If { cond, then, or_else }.with_span(span)))
        }

        /// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
        fn parse_for_expr(&mut self) -> Result<ast::Expr> {
            trace!("parsing for expression, cur_token = {:?}", self.cur_token);

            let for_span = eat_token!(self, |Token::For, span| => { span } | "Expected `for`");

            let var_name = match_token!(self, Token::Identifier(name) => {
                name
            } | "Expected `identifier` after `for`");

            self.next_token(); // eat the identifier.

            eat_token!(self, Token::Character('=') | "Expected `=` after `for`");

            let start = Box::new(self.parse_expression()?);

            eat_token!(self, Token::Character(',') | "Expected `,` after `for`");

            let end = Box::new(self.parse_expression()?);

            // The step value is optional.
            let step = match_token!(self,
                Token::Character(',') => {
                    self.next_token(); // eat the `,`.

                    Some(Box::new(self.parse_expression()?))
                } | _ => {
                    None
                }
            );

            eat_token!(self, Token::In | "Expected `in` after `for`");

            let body = Box::new(self.parse_expression()?);
            let span = for_span | body.span();

            Ok(ast::Expr::For(
                ast::For {
                    var_name,
                    start,
                    end,
                    step,
                    body,
                }
                .with_span(span),
            ))
        }

        /// varexpr ::= 'var' identifier ('=' expression)?
        //                    (',' identifier ('=' expression)?)* 'in' expression
        fn parse_var_expr(&mut self) -> Result<ast::Expr> {
            trace!("parsing var expression, cur_token = {:?}", self.cur_token);

            let var_span = eat_token!(self, |Token::Var, span| => { span } | "Expected `var`");

            let mut vars = Vec::new();

            // At least one variable name is required.
            loop {
                let var_name = match_token!(self, Token::Identifier(name) => {
                    name
                } | "Expected `identifier` after `var`");

                self.next_token(); // eat the identifier.

                // Read the optional initializer.
                let var_init = match_token!(self,
                    Token::Character('=') => {
                        self.next_token(); // eat the '='.

                        Some(self.parse_expression()?)
                    } | _ => {
                        None
                    }
                );

                vars.push((var_name, var_init));

                match_token!(self,
                    Token::Character(',') => {
                        self.next_token(); // eat the ','.
                    } | _ => {
                        break;
                    }
                );
            }

            // At this point, we have to have 'in'.
            eat_token!(self, Token::In | "Expected 'in' keyword after 'var'");

            let body = Box::new(self.parse_expression()?);
            let span = var_span | body.span();

            Ok(ast::Expr::Var(ast::Var { vars, body }.with_span(span)))
        }

        /// primary
        ///   ::= identifierexpr
        ///   ::= numberexpr
        ///   ::= parenexpr
        ///   ::= ifexpr
        ///   ::= forexpr
        ///   ::= varexpr
        fn parse_primary(&mut self) -> Result<ast::Expr> {
            trace!("parsing primary expression, cur_token = {:?}", self.cur_token);

            match_token!(self,
                Token::Identifier(_) => {
                    self.parse_identifier_expr()
                },
                Token::Number(_) => {
                    self.parse_number_expr()
                },
                Token::Character('(') => {
                    self.parse_paren_expr()
                },
                Token::If => {
                    self.parse_if_expr()
                },
                Token::For => {
                    self.parse_for_expr()
                },
                Token::Var => {
                    self.parse_var_expr()
                }
                | "Expected `identifier`, `number` or `(`")
        }

        /// unary
        ///   ::= primary
        ///   ::= '!' unary
        fn parse_unary(&mut self) -> Result<ast::Expr> {
            trace!("parsing unary expression, cur_token = {:?}", self.cur_token);

            match self.cur_token {
                Some(Spanned {
                    item: Token::Character(c),
                    mut span,
                }) if c != '(' && c != ',' => {
                    // If this is a unary operator, read it.
                    self.next_token(); // eat the unary operator

                    let operand = Box::new(self.parse_unary()?);

                    span |= operand.span();

                    Ok(ast::Expr::Unary(ast::Unary { opcode: c, operand }.with_span(span)))
                }
                _ => {
                    // If the current token is not an operator, it must be a primary expr.
                    self.parse_primary()
                }
            }
        }

        /// binoprhs
        ///   ::= ('+' primary)*
        fn parse_bin_op_rhs(&mut self, expr_prec: i32, mut lhs: ast::Expr) -> Result<ast::Expr> {
            // If this is a binop, find its precedence.
            loop {
                let tok_prec = self.get_tok_precedence().unwrap_or(-1);

                // If this is a binop that binds at least as tightly as the current binop,
                // consume it, otherwise we are done.
                if tok_prec < expr_prec {
                    debug!("parsed expression: {:?}", lhs);

                    return Ok(lhs);
                }

                // Okay, we know this is a binop.
                let op = self.cur_token.as_ref().unwrap().as_bin_op().unwrap();

                self.next_token(); // eat binop

                // Parse the unary expression after the binary operator.
                let mut rhs = self.parse_unary()?;

                // If BinOp binds less tightly with RHS than the operator after RHS,
                // let the pending operator take RHS as its LHS.
                let next_prec = self.get_tok_precedence().unwrap_or(-1);

                if tok_prec < next_prec {
                    rhs = self.parse_bin_op_rhs(tok_prec + 1, rhs)?;
                }

                let span = lhs.span() | rhs.span();

                lhs = ast::Expr::Binary(
                    ast::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                    .with_span(span),
                )
            }
        }

        /// expression
        ///   ::= unary binoprhs
        ///
        fn parse_expression(&mut self) -> Result<ast::Expr> {
            trace!("parsing expression, cur_token = {:?}", self.cur_token);

            let lhs = self.parse_unary()?;

            self.parse_bin_op_rhs(0, lhs)
        }

        /// prototype
        ///   ::= id '(' id* ')'
        ///   ::= binary LETTER number? (id, id)
        ///   ::= unary LETTER (id)
        ///
        /// ```no_run
        /// # Logical unary not.
        /// def unary!(v)
        ///     if v then
        ///         0
        ///     else
        ///         1;
        ///
        /// # Binary "logical or", (note that it does not "short circuit")
        /// def binary| 5 (LHS RHS)
        ///     if LHS then
        ///         1
        ///     else if RHS then
        ///         1
        ///     else
        ///         0;
        /// ```
        fn parse_prototype(&mut self) -> Result<Spanned<ast::Prototype>> {
            trace!("parsing prototype, cur_token = {:?}", self.cur_token);

            let (name, name_span, is_operator, precedence) = match_token!(self,
                |Token::Identifier(ref name), span| => {
                    (name.clone(), span, false,  None)
                },
                |Token::UnaryOp, span| => {
                    self.next_token(); // Eat the `id`

                    let op = eat_token!(self, Token::Character(op) => {
                        op
                    } | "Expected unary operator");

                    (format!("unary{}", op), span, true, None)
                },
                |Token::BinaryOp, span| => {
                    self.next_token(); // Eat the `id`

                    let op = eat_token!(self, Token::Character(op) => {
                        op
                    } | "Expected binary operator");

                    let precedence = match_token!(self,
                        Token::Number(n) => {
                            self.next_token(); // Eat the `precedence`

                            Some(n as isize)
                        }
                        | _ => { None }
                    );

                    (format!("binary{}", op), span, true, precedence)
                }
                | "Expected `(` in prototype");

            if !is_operator {
                self.next_token(); // Eat the `id`
            }

            match_token!(self, Token::Character('(') => {
                // ignore it
            } | "Expected `(` in prototype");

            let mut args = vec![];

            while let Token::Identifier(name) = self.next_token().item {
                args.push(name);
            }

            let end_span = eat_token!(self, |Token::Character(')'), span| => { span } | "Expected `)` in prototype");

            let proto = ast::Prototype {
                name,
                args,
                is_operator,
                precedence,
            }
            .with_span(name_span | end_span);

            debug!("parsed prototype: {:?}", proto);

            Ok(proto)
        }

        /// definition ::= 'def' prototype expression
        pub fn parse_definition(&mut self) -> Result<Spanned<ast::Function>> {
            trace!("parsing definition, cur_token = {:?}", self.cur_token);

            let def_span = eat_token!(self, |Token::Def, span| => { span } | "Expected `def` in definition");

            let proto = self.parse_prototype()?;
            let body = self.parse_expression()?;
            let span = def_span | proto.span | body.span();

            Ok(ast::Function { proto, body }.with_span(span))
        }

        /// toplevelexpr ::= expression
        pub fn parse_top_level_expr(&mut self) -> Result<Spanned<ast::Function>> {
            trace!("parsing toplevelexpr, cur_token = {:?}", self.cur_token);

            let expr = self.parse_expression()?;
            let span = expr.span();

            Ok(ast::Function {
                proto: ast::Prototype {
                    name: "main".to_owned(),
                    args: vec![],
                    is_operator: false,
                    precedence: None,
                }
                .with_span(span),
                body: expr,
            }
            .with_span(span))
        }

        /// external ::= 'extern' prototype
        pub fn parse_extern(&mut self) -> Result<Spanned<ast::Prototype>> {
            trace!("parsing extern, cur_token = {:?}", self.cur_token);

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
    use std::path::Path;
    use std::rc::Rc;

    use llvm::debuginfo::LLVMDIFlags;

    use jit;
    use jit::debuginfo::*;
    use jit::insts::*;
    use jit::prelude::*;

    use crate::ast;
    use crate::dbginfo::DebugInfo;
    use crate::errors::{ErrorKind, Result};
    use crate::lexer::{Span, Spanned};

    pub struct CodeGenerator<'a> {
        pub context: &'a Context,
        pub module: Module,
        pub builder: IRBuilder,
        pub named_values: HashMap<String, AllocaInst>,
        pub protos: Rc<RefCell<HashMap<String, Spanned<ast::Prototype>>>>,
        pub binop_precedences: Rc<RefCell<HashMap<String, isize>>>,
        pub dbg_info: DebugInfo<'a>,
    }

    impl<'a> CodeGenerator<'a> {
        pub fn build(self) -> Module {
            self.dbg_info.build();

            self.module
        }

        fn get_function(&mut self, name: &str) -> Option<Function> {
            // First, check for an existing function from a previous 'extern' declaration.
            self.module.get_function(name).or_else(|| {
                // If not, check whether we can codegen the declaration from some existing prototype.
                let proto = self.protos.borrow().get(name).cloned();

                proto.map(|proto| {
                    trace!("construct function base on prototype `{}`", name);

                    proto.codegen(self).unwrap().into()
                })
            })
        }

        /// Create an alloca instruction in the entry block of the function.
        ///
        /// This is used for mutable variables etc.
        fn create_entry_block_alloca(&mut self, func: &Function, var_name: &str) -> AllocaInst {
            let builder = self.context.create_builder();

            let entry = func.entry().unwrap();
            let instr = entry.instructions().next();

            builder.position_to(entry, instr);

            alloca!(self.context.double_t(); var_name).emit_to(&builder)
        }

        pub fn emit_location(&self, item: Option<Span>) {
            let loc = self.dbg_info.create_debug_location(item);

            self.builder.set_current_debug_location(loc, self.context)
        }

        pub fn insert_declare_at_end<V: Into<ValueRef>>(
            &self,
            init_val: V,
            var: DILocalVariable,
            span: Span,
        ) -> Instruction {
            self.builder
                .emit(self.dbg_info.insert_declare_at_end(
                    init_val.into(),
                    var,
                    span,
                    self.builder.insert_block().unwrap(),
                ))
                .into()
        }
    }

    pub fn new<'a, N: AsRef<str>, P: AsRef<Path>>(
        context: &'a Context,
        name: N,
        path: P,
        protos: Rc<RefCell<HashMap<String, Spanned<ast::Prototype>>>>,
        binop_precedences: Rc<RefCell<HashMap<String, isize>>>,
    ) -> CodeGenerator<'a> {
        // Open a new module.
        let module = context.create_module(name);
        let dbg_info = DebugInfo::new(context, &module, path);

        CodeGenerator {
            context,
            module,
            builder: context.create_builder(),
            named_values: HashMap::new(),
            protos,
            binop_precedences,
            dbg_info,
        }
    }

    pub trait CodeGen {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef>;
    }

    impl CodeGen for ast::Expr {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            match self {
                ast::Expr::Number(expr) => expr.codegen(gen),
                ast::Expr::Variable(expr) => expr.codegen(gen),
                ast::Expr::Unary(expr) => expr.codegen(gen),
                ast::Expr::Binary(expr) => expr.codegen(gen),
                ast::Expr::Call(expr) => expr.codegen(gen),
                ast::Expr::If(expr) => expr.codegen(gen),
                ast::Expr::For(expr) => expr.codegen(gen),
                ast::Expr::Var(expr) => expr.codegen(gen),
            }
        }
    }

    impl CodeGen for Spanned<ast::Number> {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            gen.emit_location(Some(self.span));

            Ok(gen.context.double_t().real(self.val).into())
        }
    }

    impl CodeGen for Spanned<ast::Variable> {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            trace!("gen code for {:?}", self);

            // Look this variable up in the function.
            if let Some(v) = gen.named_values.get(&self.name) {
                gen.emit_location(Some(self.span));

                // Load the value.
                Ok(load!(*v; self.name.as_str()).emit_to(&gen.builder).into())
            } else {
                bail!(ErrorKind::UnknownVariable(self.name.clone()))
            }
        }
    }

    impl CodeGen for Spanned<ast::Unary> {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            trace!("gen code for {:?}", self);

            if let Some(func) = gen.get_function(self.function_name().as_str()) {
                let operand = self.operand.codegen(gen)?;

                gen.emit_location(Some(self.span));

                Ok(call!(func, operand; "unop").emit_to(&gen.builder).into())
            } else {
                bail!(ErrorKind::UnknownFunction(self.function_name()))
            }
        }
    }

    impl CodeGen for Spanned<ast::Binary> {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            trace!("gen code for {:?}", self);

            gen.emit_location(Some(self.span));

            // Special case '=' because we don't want to emit the LHS as an expression.
            if self.op == ast::BinOp::Assignment {
                // Assignment requires the LHS to be an identifier.
                if let ast::Expr::Variable(var) = &*self.lhs {
                    // Codegen the RHS.
                    let val = self.rhs.codegen(gen)?;

                    // Look up the name.
                    if let Some(var) = gen.named_values.get(&var.name) {
                        gen.builder <<= store!(val, var.clone());

                        Ok(val.into())
                    } else {
                        bail!(ErrorKind::UnknownVariable(var.name.clone()))
                    }
                } else {
                    bail!("destination of '=' must be a variable")
                }
            } else {
                let f64_t = gen.context.double_t();

                let lhs = self.lhs.codegen(gen)?;
                let rhs = self.rhs.codegen(gen)?;

                Ok(match self.op {
                    ast::BinOp::Assignment => unreachable!(),
                    ast::BinOp::Add => fadd!(lhs, rhs; "addtmp").emit_to(&gen.builder).into(),
                    ast::BinOp::Sub => fsub!(lhs, rhs; "subtmp").emit_to(&gen.builder).into(),
                    ast::BinOp::Mul => fmul!(lhs, rhs; "multmp").emit_to(&gen.builder).into(),
                    ast::BinOp::LessThen => {
                        let lhs = fult!(lhs, rhs; "cmptmp");
                        // Convert bool 0/1 to double 0.0 or 1.0
                        uitofp!(lhs, f64_t; "booltmp").emit_to(&gen.builder).into()
                    }
                    ast::BinOp::UserDefined(_) => {
                        // If it wasn't a builtin binary operator, it must be a user defined one.
                        if let Some(func) = gen.get_function(self.function_name().as_str()) {
                            // Emit a call to it.
                            call!(func, lhs, rhs).emit_to(&gen.builder).into()
                        } else {
                            bail!(ErrorKind::UnknownFunction(self.function_name()))
                        }
                    }
                })
            }
        }
    }

    impl CodeGen for Spanned<ast::Call> {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            trace!("gen code for {:?}", self);

            // Look up the name in the global module table.
            if let Some(func) = gen.get_function(&self.callee) {
                // If argument mismatch error.
                if self.args.len() != func.param_count() {
                    bail!(ErrorKind::IncorrectArguments(self.args.len(), func.param_count(),))
                }

                let mut args = vec![];

                for arg in &self.args {
                    args.push(arg.codegen(gen)?);
                }

                gen.emit_location(Some(self.span));

                Ok(call(func, args, "calltmp").emit_to(&gen.builder).into())
            } else {
                bail!(ErrorKind::UnknownFunction(self.callee.clone()))
            }
        }
    }

    impl CodeGen for Spanned<ast::If> {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            trace!("gen code for {:?}", self);

            gen.emit_location(Some(self.span));

            let f64_t = gen.context.double_t();

            // Convert condition to a bool by comparing non-equal to 0.0.
            let cond = one!(self.cond.codegen(gen)?, f64_t.real(0.0));

            let func = gen.builder.insert_block().unwrap().parent();

            // Create blocks for the then and else cases.
            // Insert the 'then' block at the end of the function.
            let then_bb = func.append_basic_block_in_context("then", gen.context);
            let else_bb = func.append_basic_block_in_context("else", gen.context);
            let merge_bb = func.append_basic_block_in_context("ifcont", gen.context);

            gen.builder <<= br!(cond => then_bb, _ => else_bb);

            // Emit then value.
            gen.builder.position_at_end(then_bb);

            gen.dbg_info.enter_lexical_block(self.then.span());

            let then = self.then.codegen(gen)?;

            gen.builder <<= br!(merge_bb);

            gen.dbg_info.leave_lexical_block();

            // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
            let then_bb = gen.builder.insert_block().unwrap();

            // Emit else block.
            gen.builder.position_at_end(else_bb);

            gen.dbg_info.enter_lexical_block(self.or_else.span());

            let or_else = self.or_else.codegen(gen)?;

            gen.builder <<= br!(merge_bb);

            gen.dbg_info.leave_lexical_block();

            // codegen of 'Else' can change the current block, update ElseBB for the PHI.
            let else_bb = gen.builder.insert_block().unwrap();

            // Emit merge block.
            gen.builder.position_at_end(merge_bb);

            let pn = phi!(f64_t, then => then_bb, or_else => else_bb; "iftmp").emit_to(&gen.builder);

            Ok(pn.into())
        }
    }

    impl CodeGen for Spanned<ast::For> {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            trace!("gen code for {:?}", self);

            gen.emit_location(Some(self.span));

            let f64_t = gen.context.double_t();

            // Make the new basic block for the loop header, inserting after current block.
            let func = gen.builder.insert_block().unwrap().parent();

            // Create an alloca for the variable in the entry block.
            let alloca = gen.create_entry_block_alloca(&func, &self.var_name);

            // Emit the start code first, without 'variable' in scope.
            let start_val = self.start.codegen(gen)?;

            // Store the value into the alloca.
            gen.builder <<= store!(start_val, alloca);

            // Make the new basic block for the loop header, inserting after current block.
            let loop_bb = func.append_basic_block_in_context("loop", gen.context);

            // Insert an explicit fall through from the current block to the LoopBB.
            gen.builder <<= br!(loop_bb);

            // Start insertion in LoopBB.
            gen.builder.position_at_end(loop_bb);

            // Within the loop, the variable is defined equal to the PHI node.
            // If it shadows an existing variable, we have to restore it, so save it now.
            let old_value = gen.named_values.insert(self.var_name.clone(), alloca.into());

            gen.dbg_info.enter_lexical_block(self.body.span());

            // Emit the body of the loop.
            // This, like any other expr, can change the current BB.
            // Note that we ignore the value computed by the body, but don't allow an error.
            self.body.codegen(gen)?;

            gen.dbg_info.leave_lexical_block();

            // Emit the step value.
            let step_val = if let Some(ref step) = self.step {
                step.codegen(gen)?
            } else {
                f64_t.real(1.0).into()
            };

            // Compute the end condition.
            let end_val = self.end.codegen(gen)?;

            // Reload, increment, and restore the alloca.
            // This handles the case where the body of the loop mutates the variable.
            let cur_val = load!(alloca; self.var_name.as_str());
            let next_val = fadd!(cur_val, step_val; "nextvar");
            gen.builder <<= store!(next_val, alloca);

            // Convert condition to a bool by comparing non-equal to 0.0.
            let end_cond = one!( end_val, f64_t.real(0.0); "loopcond");

            // Create the "after loop" block and insert it.
            let after_bb = func.append_basic_block_in_context("afterloop", gen.context);

            // Insert the conditional branch into the end of LoopEndBB.
            gen.builder <<= br!(end_cond => loop_bb, _ => after_bb);

            // Any new code will be inserted in AfterBB.
            gen.builder.position_at_end(after_bb);

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

    impl CodeGen for Spanned<ast::Var> {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            trace!("gen code for {:?}", self);

            let f64_t = gen.context.double_t();

            let func = gen.builder.insert_block().unwrap().parent();

            let mut old_bindings = Vec::new();

            // Register all variables and emit their initializer.
            for &(ref var_name, ref var_init) in &self.vars {
                // Emit the initializer before adding the variable to scope,
                // this prevents the initializer from referencing the variable itself,
                // and permits stuff like this:
                //  var a = 1 in
                //    var a = a in ...   # refers to outer 'a'.
                let init_val = if let Some(init) = var_init.as_ref() {
                    init.codegen(gen)?
                } else {
                    f64_t.real(0.0).into()
                };

                let alloca = gen.create_entry_block_alloca(&func, var_name);

                let var = gen.dbg_info.create_auto_variable(var_name, self.span.start.line);

                gen.builder <<= store!(init_val, alloca);

                gen.insert_declare_at_end(init_val, var, self.span);

                // Remember the old variable binding so that we can restore the binding when we unrecurse.
                old_bindings.push(gen.named_values.insert(var_name.clone(), alloca));
            }

            gen.emit_location(Some(self.span));

            // Codegen the body, now that all vars are in scope.
            let body_val = self.body.codegen(gen)?;

            // Pop all our variables from scope.
            for (&(ref var_name, _), &old_binding) in self.vars.iter().zip(old_bindings.iter()) {
                if let Some(value) = old_binding {
                    gen.named_values.insert(var_name.clone(), value);
                }
            }

            Ok(body_val.into())
        }
    }

    impl CodeGen for Spanned<ast::Prototype> {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            trace!("gen code for {:?}", self);

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

    impl CodeGen for Spanned<ast::Function> {
        fn codegen(&self, gen: &mut CodeGenerator) -> Result<ValueRef> {
            trace!("gen code for {:?}", self);

            gen.protos
                .borrow_mut()
                .insert(self.proto.name.clone(), self.proto.clone());

            let func = if let Some(func) = gen.get_function(&self.proto.name) {
                func
            } else {
                bail!(ErrorKind::UnknownFunction(self.proto.name.clone()))
            };

            if let Some(precedence) = self.proto.precedence {
                gen.binop_precedences
                    .borrow_mut()
                    .insert(self.proto.name.clone(), precedence);
            }

            // Create a new basic block to start insertion into.
            let bb = func.append_basic_block_in_context("entry", &gen.context);
            gen.builder.position_at_end(bb);

            // Create a subprogram DIE for this function.
            let unit = gen.dbg_info.file;
            let line_no = self.proto.span.start.line as u32;
            let func_ty = gen.dbg_info.create_func_ty(func.param_count());
            let sp = gen
                .dbg_info
                .create_function_builder(gen.dbg_info.file, &self.proto.name, unit, line_no, func_ty, line_no)
                .with_flags(LLVMDIFlags::LLVMDIFlagPrototyped)
                .build();
            func.set_subprogram(sp);

            // Push the current scope.
            gen.dbg_info.lexical_blocks.push(sp.into());

            // Unset the location for the prologue emission (leading instructions with no
            // location in a function are considered part of the prologue and the debugger
            // will run past them when breaking on a function)
            gen.emit_location(None);

            // Record the function arguments in the NamedValues map.
            gen.named_values = func
                .params()
                .enumerate()
                .map(|(pos, arg)| {
                    let arg_name = String::from(arg.name().unwrap());

                    // Create an alloca for this variable.
                    let alloca = gen.create_entry_block_alloca(&func, &arg_name);

                    // Create a debug descriptor for the variable.
                    let var = gen
                        .dbg_info
                        .create_parameter_variable(&arg_name, pos as u32 + 1, line_no as usize);

                    // Store the initial value into the alloca.
                    gen.builder <<= store!(arg, alloca);

                    gen.insert_declare_at_end(alloca, var, self.proto.span);

                    // Add arguments to variable symbol table.
                    (arg_name, alloca)
                })
                .collect::<HashMap<String, AllocaInst>>();

            trace!("{} params: {:?}", gen.named_values.len(), gen.named_values);

            let ret_val = self.body.codegen(gen)?;

            gen.emit_location(Some(self.body.span()));

            // Finish off the function.
            gen.builder <<= ret!(ret_val);

            // Validate the generated code, checking for consistency.
            if func.verify().is_err() {
                gen.module.verify()?;
            }

            // Pop off the lexical block for the function.
            gen.dbg_info.lexical_blocks.pop();

            Ok(func.into())
        }
    }
}

use std::cell::RefCell;
use std::char;
use std::collections::HashMap;
use std::rc::Rc;

use jit::prelude::*;
use jit::target::*;

use crate::codegen::{CodeGen, CodeGenerator};
use crate::errors::Result;
use crate::lexer::{Spanned, Token};

//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

impl<I> parser::Parser<I>
where
    I: Iterator<Item = char>,
{
    fn handle_definition(&mut self, gen: &mut CodeGenerator, _engine: &mut KaleidoscopeJIT) -> Result<ValueRef> {
        self.parse_definition().and_then(move |func| {
            debug!("parsed a function difinition: {:?}", func);

            let ir = func.codegen(gen)?;

            debug!("generate function definition:\n{}", ir);

            Ok(ir)
        })
    }

    fn handle_extern(&mut self, gen: &mut CodeGenerator, _engine: &mut KaleidoscopeJIT) -> Result<ValueRef> {
        self.parse_extern().and_then(|proto| {
            debug!("parsed a extern prototype: {:?}", proto);

            let ir = proto.codegen(gen)?;

            debug!("generate extern:\n{}", ir);

            gen.protos.borrow_mut().insert(proto.name.clone(), proto);

            Ok(ir)
        })
    }

    fn handle_top_level_expression(
        &mut self,
        gen: &mut CodeGenerator,
        _engine: &mut KaleidoscopeJIT,
    ) -> Result<ValueRef> {
        self.parse_top_level_expr().and_then(move |func| {
            debug!("parsed a top level expression: {:?}", func);

            let ir = func.codegen(gen)?;

            debug!("generate top level expression:\n{}", ir);

            Ok(ir)
        })
    }

    fn handle_top(&mut self, gen: &mut CodeGenerator, engine: &mut KaleidoscopeJIT) -> Result<Parsed> {
        // top ::= definition | external | expression | ';'
        match self.next_token() {
            Spanned { item: Token::Def, span } => {
                trace!("handle definition @ {}", span);

                Ok(Parsed::Code(self.handle_definition(gen, engine)?))
            }
            Spanned {
                item: Token::Extern,
                span,
            } => {
                trace!("handle extern @ {}", span);

                Ok(Parsed::Code(self.handle_extern(gen, engine)?))
            }
            Spanned { item: Token::Eof, span } => {
                trace!("handle EOF @ {}", span);

                Ok(Parsed::ToEnd)
            }
            token @ Spanned {
                item: Token::Character(';'),
                ..
            }
            | token @ Spanned {
                item: Token::Comment(_),
                ..
            } => Ok(Parsed::Skipped(token)),
            Spanned { item, span } => {
                trace!("handle {:?} @ {}", item, span);

                Ok(Parsed::Code(self.handle_top_level_expression(gen, engine)?))
            }
        }
    }
}

struct KaleidoscopeJIT {
    pub modules: Vec<jit::ModuleHandle>,
    pub protos: Rc<RefCell<HashMap<String, Spanned<ast::Prototype>>>>,
}

impl KaleidoscopeJIT {
    pub fn new() -> Result<KaleidoscopeJIT> {
        jit::Symbols::load_current_exe()?;

        Ok(KaleidoscopeJIT {
            modules: Vec::new(),
            protos: Rc::new(RefCell::new(HashMap::new())),
        })
    }
}

enum Parsed {
    ToEnd,
    Code(ValueRef),
    Skipped(Spanned<Token>),
}

//===----------------------------------------------------------------------===//
// Debug Information
//===----------------------------------------------------------------------===//

mod dbginfo {
    use std::iter;
    use std::ops::Deref;
    use std::path::Path;

    use llvm::debuginfo::{LLVMDIFlags, LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC};

    use jit::debuginfo::*;
    use jit::prelude::*;

    use crate::lexer::Span;

    pub struct DebugInfo<'a> {
        pub context: &'a Context,
        pub builder: DIBuilder,
        pub file: DIFile,
        pub compile_unit: DICompileUnit,
        pub double_ty: DIBasicType,
        pub lexical_blocks: Vec<DIScope>,
    }

    impl<'a> Deref for DebugInfo<'a> {
        type Target = DIBuilder;

        fn deref(&self) -> &Self::Target {
            &self.builder
        }
    }

    impl<'a> DebugInfo<'a> {
        pub fn new<P: AsRef<Path>>(context: &'a Context, m: &Module, path: P) -> Self {
            let builder = m.create_di_builder();

            // Construct the DIBuilder
            let file = builder.create_file(path);

            // Create the compile unit for the module.
            let compile_unit = builder.create_compile_unit(LLVMDWARFSourceLanguageC, file, "Kaleidoscope Compiler");

            let double_ty = builder.create_basic_type("double", 64, encoding::FLOAT);

            DebugInfo {
                context,
                builder,
                file,
                compile_unit,
                double_ty,
                lexical_blocks: vec![],
            }
        }

        pub fn build(&self) {
            self.builder.finalize()
        }

        pub fn create_func_ty(&self, argc: usize) -> DISubroutineType {
            self.builder.create_subroutine_type(
                self.file,
                iter::repeat(self.double_ty.into()).take(argc + 1),
                LLVMDIFlags::LLVMDIFlagZero,
            )
        }

        pub fn create_debug_location(&self, item: Option<Span>) -> DILocation {
            let (span, scope) = item.map(|span| (span, self.lexical_blocks.last())).unwrap_or_default();

            let loc = self.context.create_debug_location(
                span.start.line as u32,
                span.start.column as u32,
                scope.unwrap_or(&self.compile_unit),
                None,
            );

            trace!("create debug location @ {}", loc);

            loc
        }

        pub fn enter_lexical_block(&mut self, span: Span) {
            self.lexical_blocks.push(
                self.create_lexical_block(
                    self.lexical_blocks.last().unwrap(),
                    self.file,
                    span.start.line as u32,
                    span.start.column as u32,
                )
                .into(),
            )
        }

        pub fn leave_lexical_block(&mut self) {
            self.lexical_blocks.pop();
        }

        pub fn create_parameter_variable(&self, name: &str, arg_no: u32, line: usize) -> DILocalVariable {
            self.builder.create_parameter_variable(
                self.lexical_blocks.last().unwrap(),
                name,
                arg_no,
                self.file,
                line as u32,
                self.double_ty,
            )
        }

        pub fn create_auto_variable(&self, name: &str, line: usize) -> DILocalVariable {
            self.builder.create_auto_variable(
                self.lexical_blocks.last().unwrap(),
                name,
                self.file,
                line as u32,
                self.double_ty,
            )
        }

        pub fn insert_declare_at_end<V: Into<ValueRef>>(
            &self,
            init_val: V,
            var: DILocalVariable,
            span: Span,
            bb: BasicBlock,
        ) -> Instruction {
            self.builder.insert_declare_at_end(
                init_val.into(),
                var,
                self.builder.create_expression(&[]),
                self.create_debug_location(Some(span)),
                bb,
            )
        }
    }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

fn main() -> Result<()> {
    pretty_env_logger::init();

    NativeTarget::init()?;
    NativeAsmParser::init()?;
    NativeAsmPrinter::init()?;

    jit::MCJITCompiler::link_in();

    let context = Context::new();

    let mut engine = KaleidoscopeJIT::new()?;

    let mut parser = parser::new(lines::Lines::new(None));
    let binop_precedences = parser.binop_precedences.clone();

    let mut gen = codegen::new(
        &context,
        "my cool jit",
        "kaleidoscope_chapter9.rs",
        engine.protos.clone(),
        binop_precedences.clone(),
    );

    // Run the main "interpreter loop" now.
    loop {
        match parser.handle_top(&mut gen, &mut engine) {
            Ok(Parsed::ToEnd) => {
                break;
            }
            Ok(Parsed::Skipped(token)) => trace!("skipped {:?}", token),
            Ok(_) => {}
            Err(err) => match parser.next_token() {
                Spanned { item: Token::Eof, .. } => break,
                token => {
                    warn!("skip token {:?} for error recovery, {}", token, err);
                }
            },
        }
    }

    println!("{}", gen.build());

    Ok(())
}
