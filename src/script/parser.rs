use super::ast::{Arm, Bind, Decl, DeclKind, Expr, ExprKind, Pattern, PatternKind, Type, Variant};
use lib::Pos;
use std::{iter, result};

#[derive(Clone, Debug)]
pub enum Error {
    UnknownChar(Pos, char),
    PartialDedent(Pos),
    Expecting(String, Option<Token>),
}

pub type Result<T> = result::Result<T, Error>;

pub fn parse(s: &str) -> Result<Vec<Decl>> {
    let tokens = Lexer::new(s.chars());
    // for token in tokens {
    //     println!("{:?}", token);
    // }
    // let tokens = Lexer::new(s.chars());
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
    Let,
    Which,
    Of,
    Colon,
    Equal,
    Arrow,
    Bar,
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
    indents: Vec<usize>,
    pending_dedents: usize,
    pending_eol: bool,
}

impl<Iter> Lexer<Iter>
where
    Iter: Iterator<Item = char>,
{
    fn new(source: Iter) -> Self {
        Lexer {
            source: source.peekable(),
            pos: Pos::new(),
            indents: Vec::new(),
            pending_dedents: 0,
            pending_eol: false,
        }
    }

    fn indent(&self) -> usize {
        self.indents.last().cloned().unwrap_or(0)
    }

    fn peek(&mut self) -> Option<char> {
        self.source.peek().cloned()
    }

    fn read(&mut self) -> Option<char> {
        let ch = self.source.next()?;
        if ch == '\n' {
            self.pos.line += 1;
            self.pos.column = 1;
        } else {
            self.pos.column += 1;
        }
        Some(ch)
    }

    fn ignore(&mut self) {
        self.read().expect("peek");
    }

    fn read_while<F>(&mut self, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut s = String::new();
        while self.peek().map_or(false, &f) {
            s.push(self.read().expect("peek"));
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
        let token = move |kind| Some(Ok(Token { pos, kind }));

        if self.pending_eol {
            self.pending_eol = false;
            return token(TokenKind::Newline);
        }
        if self.pending_dedents > 0 {
            self.pending_dedents -= 1;
            self.pending_eol = self.pending_dedents == 0;
            return token(TokenKind::Dedent);
        }

        let ch = match self.peek() {
            None => {
                self.pending_dedents = self.indents.len();
                return if self.pending_dedents > 0 {
                    self.next()
                } else {
                    None
                };
            }
            Some(ch) => ch,
        };

        let kind = if ch.is_alphabetic() || ch == '_' {
            let s = self.read_while(|c| c.is_alphanumeric() || c == '_');
            match s.as_ref() {
                "def" => TokenKind::Def,
                "let" => TokenKind::Let,
                "which" => TokenKind::Which,
                "of" => TokenKind::Of,
                _ => TokenKind::Id(s),
            }
        } else {
            self.ignore();
            let next = self.peek();
            match (ch, next) {
                (':', _) => TokenKind::Colon,
                ('=', _) => TokenKind::Equal,
                ('|', _) => TokenKind::Bar,
                ('(', _) => TokenKind::LeftParen,
                (')', _) => TokenKind::RightParen,
                ('-', Some('>')) => {
                    self.ignore();
                    TokenKind::Arrow
                }
                ('\n', _) => {
                    let mut ind = 0;
                    while self.peek() == Some(' ') {
                        ind += 1;
                        self.ignore();
                    }

                    if self.peek() == Some('\n') {
                        return self.next();
                    }

                    let last = self.indent();
                    if ind < last {
                        while ind < self.indent() {
                            self.indents.pop().expect("last");
                            self.pending_dedents += 1;
                        }
                        if ind != self.indent() {
                            return Some(Err(Error::PartialDedent(pos)));
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
                _ => return Some(Err(Error::UnknownChar(pos, ch))),
            }
        };

        token(kind)
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

    fn ignore(&mut self) {
        self.next().expect("peek").expect("peek");
    }

    fn satisfy<F>(&mut self, f: F) -> Result<bool>
    where
        F: FnOnce(&TokenKind) -> bool,
    {
        Ok(self.peek()?.map_or(false, |t| f(&t.kind)))
    }

    fn matches(&mut self, kind: &TokenKind) -> Result<bool> {
        self.satisfy(|k| k == kind)
    }

    fn consume(&mut self, kind: &TokenKind) -> Result<bool> {
        let res = self.matches(kind)?;
        if res {
            self.ignore();
        }
        Ok(res)
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<()> {
        if self.consume(kind)? {
            Ok(())
        } else {
            let expect = format!("{:?}", kind);
            let got = self.next().expect("peek");
            Err(Error::Expecting(expect, got))
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
        self.expect(&TokenKind::Def)?;
        let name = self.id()?;

        let kind = if is_type(&name) {
            self.expect(&TokenKind::Equal)?;

            self.expect(&TokenKind::Indent)?;
            let variants = self.sep_by(Self::variant, &TokenKind::Newline)?;
            self.expect(&TokenKind::Dedent)?;

            let tp = Type { variants };

            DeclKind::Type(tp)
        } else {
            let params = self.pattern_atoms()?;
            self.expect(&TokenKind::Equal)?;
            let body = self.expr()?;

            let mut value = body;
            for param in params.into_iter().rev() {
                value = Expr {
                    kind: ExprKind::Func(param, Box::new(value)),
                }
            }

            DeclKind::Bind(value)
        };

        self.expect(&TokenKind::Newline)?;

        Ok(Decl { name, kind })
    }

    fn variant(&mut self) -> Result<Variant> {
        let name = self.id()?;
        let fields = self.ids()?;
        Ok(Variant { name, fields })
    }

    fn block(&mut self) -> Result<Expr> {
        let kind = if self.consume(&TokenKind::Let)? {
            let pat = self.pattern()?;
            self.expect(&TokenKind::Equal)?;
            let value = self.expr()?;
            self.expect(&TokenKind::Newline)?;

            let next = self.block()?;

            let func = Expr {
                kind: ExprKind::Func(pat, Box::new(next)),
            };
            ExprKind::Apply(Box::new(func), Box::new(value))
        } else {
            let value = self.expr()?;

            let pat = match self.peek()?.map(|t| &t.kind) {
                Some(TokenKind::Arrow) => {
                    self.ignore();
                    let pat = self.pattern()?;
                    self.expect(&TokenKind::Newline)?;
                    pat
                }
                Some(TokenKind::Newline) => {
                    self.ignore();
                    Pattern {
                        kind: PatternKind::Ignore,
                    }
                }
                _ => return Ok(value),
            };

            let next = self.block()?;

            let func = Expr {
                kind: ExprKind::Func(pat, Box::new(next)),
            };
            ExprKind::Seq(Box::new(value), Box::new(func))
        };

        Ok(Expr { kind })
    }

    fn expr(&mut self) -> Result<Expr> {
        let mut expr = self.atom()?;

        while self.satisfy(|k| match k {
            TokenKind::Id(_) | TokenKind::LeftParen | TokenKind::Indent | TokenKind::Which => true,
            _ => false,
        })? {
            let val = self.atom()?;
            expr = Expr {
                kind: ExprKind::Apply(Box::new(expr), Box::new(val)),
            };
        }

        Ok(expr)
    }

    fn atom(&mut self) -> Result<Expr> {
        let expr = match self.peek()?.map(|t| &t.kind) {
            Some(TokenKind::Id(_)) => {
                let bind = self.bind()?;
                Expr {
                    kind: ExprKind::Bind(bind),
                }
            }
            Some(TokenKind::LeftParen) => {
                self.ignore();
                if self.consume(&TokenKind::RightParen)? {
                    Expr {
                        kind: ExprKind::Unit,
                    }
                } else {
                    let expr = self.expr()?;
                    self.expect(&TokenKind::RightParen)?;
                    expr
                }
            }
            Some(TokenKind::Indent) => {
                self.ignore();
                let expr = self.block()?;
                self.expect(&TokenKind::Dedent)?;
                expr
            }
            Some(TokenKind::Which) => {
                self.ignore();
                let expr = self.expr()?;
                self.expect(&TokenKind::Of)?;
                self.expect(&TokenKind::Indent)?;
                let arms = self.sep_by(Self::arm, &TokenKind::Newline)?;
                self.expect(&TokenKind::Dedent)?;
                Expr {
                    kind: ExprKind::Match(Box::new(expr), arms),
                }
            }
            _ => {
                let token = self.next().expect("peek");
                return Err(Error::Expecting("value".to_string(), token));
            }
        };
        Ok(expr)
    }

    fn arm(&mut self) -> Result<Arm> {
        let pattern = self.pattern()?;
        self.expect(&TokenKind::Bar)?;
        let value = self.expr()?;
        Ok(Arm { pattern, value })
    }

    fn pattern(&mut self) -> Result<Pattern> {
        let is_type = self.satisfy(|k| match k {
            TokenKind::Id(name) => is_type(name),
            _ => false,
        })?;

        if is_type {
            let tp = self.id()?;
            let fields = self.pattern_atoms()?;

            let kind = PatternKind::Struct(tp, fields);
            Ok(Pattern { kind })
        } else {
            self.pattern_atom()
        }
    }

    fn pattern_atoms(&mut self) -> Result<Vec<Pattern>> {
        let mut pats = Vec::new();
        while self.satisfy(|k| match k {
            TokenKind::Id(_) | TokenKind::LeftParen => true,
            _ => false,
        })? {
            let pat = self.pattern_atom()?;
            pats.push(pat);
        }
        Ok(pats)
    }

    fn pattern_atom(&mut self) -> Result<Pattern> {
        let pat = match self.peek()?.map(|t| &t.kind) {
            Some(TokenKind::Id(_)) => {
                let name = self.id()?;
                let kind = if name == "_" {
                    PatternKind::Ignore
                } else {
                    PatternKind::Bind(name)
                };
                Pattern { kind }
            }
            Some(TokenKind::LeftParen) => {
                self.ignore();
                let pat = self.pattern()?;
                self.expect(&TokenKind::RightParen)?;
                pat
            }
            _ => {
                let token = self.next()?;
                return Err(Error::Expecting("pattern".to_string(), token));
            }
        };
        Ok(pat)
    }

    fn bind(&mut self) -> Result<Bind> {
        let names = self.sep_by(Self::id, &TokenKind::Colon)?;
        Ok(Bind { names })
    }

    fn ids(&mut self) -> Result<Vec<String>> {
        let mut res = Vec::new();
        while let Some(TokenKind::Id(_)) = self.peek()?.map(|t| &t.kind) {
            let id = self.id().expect("peek");
            res.push(id);
        }
        Ok(res)
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

fn is_type(name: &str) -> bool {
    name.chars().next().map_or(false, char::is_uppercase)
}
