use std::process;
use std::str;
use std::iter;


pub enum AstNode {
    Val(String),
    List(Vec<AstNode>),
}

pub fn parse(code: &str) -> AstNode {
    let mut input_stream = InputStream::new(code);
    let mut token_stream = TokenStream::new(&mut input_stream);
    parse_tokstream(&mut token_stream)
}

fn parse_tokstream(input: &mut TokenStream) -> AstNode {
    let mut list = Vec::<AstNode>::new();
    loop {
        let tok = input.next();
        match tok {
            None => break,
            Some(Token::Close) => break,
            Some(Token::Sym(sym)) => list.push(AstNode::Val(sym)),
            Some(Token::Open) => list.push(parse_tokstream(input)),
        }
    }
    AstNode::List(list)
}

struct InputStream<'a> {
    input: iter::Peekable<str::Chars<'a>>,
    pos: u32,
    line: u32,
    col: u32,
}

impl<'a> InputStream<'a> {
    fn new(input: &'a str) -> InputStream<'a> {
        InputStream {
            input: input.chars().peekable(),
            pos: 0,
            line: 1,
            col: 0,
        }
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.input.next();
        self.pos += 1;
        if ch == Some('\n') {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        ch
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn eof(&mut self) -> bool {
        self.input.peek() == None
    }

    fn croak(&self, msg: &str) {
        eprintln!("{} ({}:{})", msg, self.line, self.col);
        process::exit(1)
    }
}

#[derive(Clone)]
enum Token {
    Open,
    Close,
    Sym(String),
}

struct TokenStream<'a> {
    input: &'a mut InputStream<'a>,
    current: Option<Token>,
}

fn is_whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\t' || ch == '\n'
}

fn is_parenthesis(ch: char) -> bool {
    ch == '(' || ch == ')'
}

fn is_symbol(ch: char) -> bool {
    !is_parenthesis(ch) && !is_whitespace(ch)
}

fn is_not_newline(ch: char) -> bool {
    ch != '\n'
}

impl<'a> TokenStream<'a> {
    fn new(input: &'a mut InputStream<'a>) -> TokenStream<'a> {
        TokenStream {
            input,
            current: None
        }
    }

    fn croak(&self, msg: &str) {
        self.input.croak(msg)
    }

    fn _read_while(&mut self, predicate: &Fn(char) -> bool) -> String {
        let mut string = String::new();
        loop {
            let ch = {
                match self.input.peek() {
                    None => break,
                    Some(ch) => *ch,
                }
            };
            if predicate(ch) {
                break;
            }
            string.push(self.input.next().unwrap())
        }
        string
    }

    fn _read_escaped(&mut self, end: char) -> String {
        let mut escaped = false;
        let mut string = String::new();
        self.input.next();
        while !self.input.eof() {
            let ch = self.input.next().unwrap();
            if escaped {
                string.push(ch);
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == end {
                break;
            } else {
                string.push(ch);
            }
        }
        string
    }

    fn _read_string(&mut self) -> Token {
        Token::Sym(self._read_escaped('"'))
    }

    fn _read_symbol(&mut self) -> Token {
        Token::Sym(self._read_while(&is_symbol))
    }

    fn _skip_comment(&mut self) {
        self._read_while(&is_not_newline);
        self.input.next();
    }

    fn _read_next(&mut self) -> Option<Token> {
        self._read_while(&is_whitespace);
        let ch = {
            match self.input.peek() {
                None => return None,
                Some(ch) => *ch,
            }
        };
        match ch {
            ';' => {
                self._skip_comment();
                self._read_next()
            },
            '"' => Some(self._read_string()),
            '(' => Some(Token::Open),
            ')' => Some(Token::Close),
            _ => {
                if is_symbol(ch) {
                    Some(self._read_symbol())
                } else {
                    self.croak("Can't handler character!");
                    None
                }
            },
        }
    }

    fn next(&mut self) -> Option<Token> {
        let tok = self.current.clone();
        self.current = None;
        match tok {
            None => self._read_next(),
            Some(_) => tok,
        }
    }
}
