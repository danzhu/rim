use super::ast::{Bind, Decl, DeclKind, Expr, ExprKind, Type};
use lib::Pos;
use std::{iter, result};

#[derive(Clone, Debug)]
pub enum Error {
    UnknownChar(char),
    Expecting(String, Option<Token>),
}

pub type Result<T> = result::Result<T, Error>;

pub fn parse(s: &str) -> Result<Vec<Decl>> {
    let tokens = Lexer::new(s.chars());
    // for token in tokens {
    //     println!("{:?}", token);
    // }
    let decls = Parser::new(tokens).parse()?;
    // for decl in decls {
    //     println!("{:?}", decl);
    // }
    Ok(decls)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pos: Pos,
    kind: TokenKind,
}

#[derive(Clone, Debug, PartialEq)]
enum TokenKind {
    Newline,
    Indent,
    Dedent,
    Def,
    Colon,
    Arrow,
    LeftParen,
    RightParen,
    Id(String),
}

struct Lexer<Iter>
where
    Iter: Iterator<Item = char>,
{
    source: iter::Peekable<Iter>,
    pos: Pos,
    indents: Vec<i32>,
    pending_dedents: usize,
}

impl<Iter> Lexer<Iter>
where
    Iter: Iterator<Item = char>,
{
    fn new(source: Iter) -> Self {
        Lexer {
            source: source.peekable(),
            pos: Pos::new(),
            indents: vec![0],
            pending_dedents: 0,
        }
    }

    fn read(&mut self) -> Option<char> {
        self.source.next().map(|ch| {
            if ch == '\n' {
                self.pos.line += 1;
                self.pos.column = 1;
            } else {
                self.pos.column += 1;
            }
            ch
        })
    }

    fn read_while<F>(&mut self, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut s = String::new();
        loop {
            match self.source.peek() {
                Some(&c) if f(c) => {
                    s.push(c);
                    self.read();
                }
                _ => break,
            }
        }
        s
    }
}

impl<Iter> Iterator for Lexer<Iter>
where
    Iter: Iterator<Item = char>,
{
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let pos = self.pos;

        if self.pending_dedents > 0 {
            self.pending_dedents -= 1;
            return Some(Ok(Token {
                pos,
                kind: TokenKind::Dedent,
            }));
        }

        let ch = match self.source.peek() {
            None => {
                self.pending_dedents = self.indents.len() - 1;
                return if self.pending_dedents > 0 {
                    self.next()
                } else {
                    None
                };
            }
            Some(&ch) => ch,
        };

        let kind = if ch.is_alphabetic() {
            let s = self.read_while(char::is_alphanumeric);
            match s.as_ref() {
                "def" => TokenKind::Def,
                _ => TokenKind::Id(s),
            }
        } else {
            self.read();
            let next = self.source.peek().cloned();
            match (ch, next) {
                (':', _) => TokenKind::Colon,
                ('(', _) => TokenKind::LeftParen,
                (')', _) => TokenKind::RightParen,
                ('-', Some('>')) => {
                    self.read().unwrap();
                    TokenKind::Arrow
                }
                ('\n', _) => {
                    let mut ind = 0;
                    while let Some(&' ') = self.source.peek() {
                        ind += 1;
                        self.read();
                    }

                    let last = self.indents.last().cloned().unwrap_or(-1);
                    if ind < last {
                        while self.indents.last().cloned().unwrap_or(-1) > ind {
                            self.indents.pop();
                            self.pending_dedents += 1;
                        }
                        return self.next();
                    } else if ind > last {
                        self.indents.push(ind);
                        TokenKind::Indent
                    } else {
                        TokenKind::Newline
                    }
                }
                _ if ch.is_whitespace() => return self.next(),
                _ => return Some(Err(Error::UnknownChar(ch))),
            }
        };

        Some(Ok(Token { pos, kind }))
    }
}

struct Parser<Iter>
where
    Iter: Iterator<Item = Result<Token>>,
{
    source: iter::Peekable<Iter>,
}

impl<Iter> Parser<Iter>
where
    Iter: Iterator<Item = Result<Token>>,
{
    fn new(source: Iter) -> Self {
        Parser {
            source: source.peekable(),
        }
    }

    fn peek(&mut self) -> Result<Option<&Token>> {
        match self.source.peek() {
            Some(Ok(token)) => Ok(Some(token)),
            Some(Err(err)) => Err(err.clone()),
            None => Ok(None),
        }
    }

    fn next(&mut self) -> Result<Option<Token>> {
        match self.source.next() {
            Some(Ok(token)) => Ok(Some(token)),
            Some(Err(err)) => Err(err),
            None => Ok(None),
        }
    }

    fn consume(&mut self, kind: &TokenKind) -> Result<bool> {
        let res = self.peek()?.map_or(false, |t| t.kind == *kind);
        if res {
            self.next()
                .expect("peek() returns Ok but next() returns Err");
        }
        Ok(res)
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<()> {
        if self.consume(kind)? {
            Ok(())
        } else {
            Err(Error::Expecting(
                format!("{:?}", kind),
                self.peek()?.cloned(),
            ))
        }
    }

    fn end_by<T, F>(&mut self, f: F, end: Option<&TokenKind>) -> Result<Vec<T>>
    where
        F: Fn(&mut Self) -> Result<T>,
    {
        let mut res = Vec::new();
        while self.peek()?.map(|t| &t.kind) != end {
            res.push(f(self)?);
        }
        Ok(res)
    }

    fn sep_by<T, F>(&mut self, f: F, sep: &TokenKind) -> Result<Vec<T>>
    where
        F: Fn(&mut Self) -> Result<T>,
    {
        let mut res = vec![f(self)?];
        while self.consume(sep)? {
            res.push(f(self)?);
        }
        Ok(res)
    }

    fn parse(&mut self) -> Result<Vec<Decl>> {
        self.end_by(Self::decl, None)
    }

    fn decl(&mut self) -> Result<Decl> {
        while self.consume(&TokenKind::Newline)? {}

        self.expect(&TokenKind::Def)?;
        let name = self.id()?;

        let kind = if name.chars().next().expect("empty id").is_uppercase() {
            let fields = self.end_by(Self::id, Some(&TokenKind::Newline))?;
            self.expect(&TokenKind::Newline)?;

            let tp = Type { fields };

            DeclKind::Type(tp)
        } else {
            let params = self.end_by(Self::id, Some(&TokenKind::Indent))?;

            self.expect(&TokenKind::Indent)?;
            let body = self.block()?;
            self.expect(&TokenKind::Dedent)?;

            let mut value = body;
            for param in params.into_iter().rev() {
                value = Expr {
                    kind: ExprKind::Func(param, Box::new(value)),
                }
            }

            DeclKind::Bind(value)
        };

        Ok(Decl { name, kind })
    }

    fn block(&mut self) -> Result<Expr> {
        let expr = self.expr()?;

        let name = if self.consume(&TokenKind::Arrow)? {
            let name = self.id()?;
            self.expect(&TokenKind::Newline)?;
            name
        } else if self.consume(&TokenKind::Newline)? {
            "_".to_string()
        } else {
            return Ok(expr);
        };

        let next = self.block()?;

        let func = Expr {
            kind: ExprKind::Func(name, Box::new(next)),
        };
        let ret = Expr {
            kind: ExprKind::Seq(Box::new(expr), Box::new(func)),
        };

        Ok(ret)
    }

    fn expr(&mut self) -> Result<Expr> {
        let mut expr = self.atom()?;

        loop {
            if let Some(Token { kind, .. }) = self.peek()? {
                match kind {
                    TokenKind::Id(_) | TokenKind::LeftParen => {}
                    _ => break,
                }
            }

            let val = self.atom()?;
            expr = Expr {
                kind: ExprKind::Apply(Box::new(expr), Box::new(val)),
            };
        }

        Ok(expr)
    }

    fn atom(&mut self) -> Result<Expr> {
        let expr = match self
            .peek()?
            .ok_or_else(|| Error::Expecting("value".to_string(), None))?
            .kind
        {
            TokenKind::Id(_) => {
                let bind = self.bind()?;
                Expr {
                    kind: ExprKind::Bind(bind),
                }
            }
            TokenKind::LeftParen => {
                self.next()?;
                let expr = self.expr()?;
                self.expect(&TokenKind::RightParen)?;
                expr
            }
            _ => return Err(Error::Expecting("value".to_string(), self.next()?)),
        };
        Ok(expr)
    }

    fn bind(&mut self) -> Result<Bind> {
        self.sep_by(Self::id, &TokenKind::Colon)
            .map(|names| Bind { names })
    }

    fn id(&mut self) -> Result<String> {
        match self.next()? {
            Some(Token {
                kind: TokenKind::Id(id),
                ..
            }) => Ok(id),
            token => Err(Error::Expecting("Id".to_string(), token)),
        }
    }
}
