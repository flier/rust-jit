//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

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
