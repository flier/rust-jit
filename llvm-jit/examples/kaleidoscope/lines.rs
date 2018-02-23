use rustyline;

pub struct Lines<'a> {
    rl: rustyline::Editor<()>,
    prompt: (&'a str, &'a str),
    line: String,
    pos: usize,
}

impl<'a> Lines<'a> {
    pub fn new() -> Self {
        Lines {
            rl: rustyline::Editor::<()>::new(),
            prompt: ("ready> ", "... "),
            line: String::default(),
            pos: 0,
        }
    }

    fn read_line(&mut self, first: bool) -> rustyline::Result<String> {
        self.rl
            .readline(if first {
                &self.prompt.0
            } else {
                &self.prompt.1
            })
            .map(|line| {
                self.rl.add_history_entry(&line);

                line
            })
    }
}

impl<'a> Iterator for Lines<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(c) = self.line.chars().skip(self.pos).next() {
                self.pos += 1;

                return Some(c);
            }

            let first_line = self.line.is_empty() || self.line.as_str().trim().ends_with(";");

            match self.read_line(first_line) {
                Ok(mut line) => {
                    line.push('\n');

                    self.line = line;
                    self.pos = 0;
                }
                Err(rustyline::error::ReadlineError::Interrupted) | Err(rustyline::error::ReadlineError::Eof) => {
                    return None
                }
                Err(err) => {
                    error!("fail to read line, {}", err);

                    return None;
                }
            }
        }
    }
}
