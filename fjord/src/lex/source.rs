use fjordtoken::payload;
use fjordtoken::token;
use lexer;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct sourcelexer<'a>
{
    pub source: &'a str,
    pub line: u64,
}

#[derive(Clone, Debug)]
pub enum error
{
    unexpectedchar(Option<char>),
}

impl<'a> sourcelexer<'a>
{
    pub fn new(source: &'a str) -> Self
    {
        sourcelexer{source, line: 1}
    }
}

impl<'a> lexer for sourcelexer<'a>
{
    type error = error;

    fn read(&mut self) -> Result<token, error>
    {
        self.skipwhitespace();
        match (self.peekchar()) {
            None =>
                Ok(self.maketoken(payload::eof)),

            Some(c) if Self::isidentifierhead(c) => {
                let token = self.readidentifierorkeyword();
                Ok(token)
            },

            _ =>
                self.readpunctuation(),
        }
    }

    fn peek(&self) -> Result<token, error>
    {
        self.clone().read()
    }
}

impl<'a> sourcelexer<'a>
{
    fn readchar(&mut self) -> Option<char>
    {
        let mut cs = self.source.chars();
        let c = cs.next();
        self.source = cs.as_str();
        self.line += (c == Some('\n')) as u64;
        c
    }

    fn peekchar(&self) -> Option<char>
    {
        self.source.chars().next()
    }

    fn maketoken(&self, payload: payload) -> token
    {
        token{payload, line: self.line}
    }

    fn iswhitespace(c: char) -> bool
    {
        c.is_whitespace()
    }

    fn isidentifierhead(c: char) -> bool
    {
        c >= 'a' && c <= 'z' ||
        c >= 'A' && c <= 'Z' ||
        c == '_'
    }

    fn isidentifiertail(c: char) -> bool
    {
        Self::isidentifierhead(c) ||
        c >= '0' && c <= '9' ||
        c == '\'' || c == '?' || c == '!'
    }

    fn skipwhitespace(&mut self)
    {
        match (self.peekchar())
        {
            Some(c) if Self::iswhitespace(c) => {
                self.readchar();
                self.skipwhitespace();
            },
            _ => (),
        }
    }

    fn readidentifierorkeyword(&mut self) -> token
    {
        let mut name = String::new();

        loop
        {
            match (self.peekchar())
            {
                Some(c) if Self::isidentifiertail(c) => {
                    self.readchar();
                    name.push(c);
                },
                _ => break,
            }
        }

        let payload = match (name.as_str()) {
            "def" => payload::kwdef,
            "end" => payload::kwend,
            "fun" => payload::kwfun,
            "sig" => payload::kwsig,
            "val" => payload::kwval,
            _ => payload::identifier(Rc::from(name)),
        };

        self.maketoken(payload)
    }

    fn readpunctuation(&mut self) -> Result<token, error>
    {
        macro_rules! punctuation {
            ($spelling:expr, $name:ident) => {
                if (self.source.starts_with($spelling))
                {
                    for _ in 0 .. $spelling.len()
                    {
                        self.readchar();
                    }
                    return Ok(self.maketoken(payload::$name));
                }
            };
        }

        punctuation!("->", puthinarrow);

        punctuation!(":", pucolon);
        punctuation!("(", puparenleft);
        punctuation!(")", puparenright);

        Err(error::unexpectedchar(self.peekchar()))
    }
}

impl fmt::Display for error
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            &error::unexpectedchar(None) => write!(f, "unexpected end of file"),
            &error::unexpectedchar(Some(c)) => write!(f, "unexpected `{}'", c),
        }
    }
}
