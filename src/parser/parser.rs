use self::Tokens::*;
use super::ast::ImportMembers;

use core::convert::From as ConvertFrom;
use lasso::Rodeo;
use lasso::Spur;
use logos::SpannedIter;
use logos::{Lexer as LogosLexer, Logos};
use peekmore::PeekMoreIterator;
use super::error::ErrKind;

use std::fmt::Debug;
use std::fmt::Display;
use std::ops::Index;
use std::ops::Range;
use std::path::PathBuf;
use peekmore::PeekMore;

// use scripting_lang::error;
// use scripting_lang::yeet;

use super::ast::Expr;
use super::ast::StrFlags;
use super::ast::StructVis;
use super::ast::TopLevel;

use crate::yeet;

pub type Token = Spanned<Tokens>;

pub struct Lexer<'l> {
    tokens: PeekMoreIterator<SpannedIter<'l, Tokens>>,
    file: Option<PathBuf>,
    pos: (usize, usize),
    source: &'l str
}

impl<'l> Lexer<'l> {
    pub fn new(source: &'l str, file: impl Into<Option<&'l PathBuf>>) -> Self {
        Lexer {
            tokens: LogosLexer::new(source).spanned().peekmore(),
            file: file.into().map(|i| i.into()),
            pos: (0, 0),
            source
        }
    }

    #[inline]
    pub fn next(&mut self) -> Option<Token> {
        let mut token = self.tokens.next()?;

        if token.0 == Whitespace {
            self.pos.1 += 1;
            token = self.tokens.next()?;
        }

        if token.0 == Newline {
            self.pos.1 = 0;
            self.pos.0 += 1;

            return Some(Token {
                data: token.0,
                span: token.1.into(),
            });
        }

        self.pos.1 += token.1.len();
        Some(Token {
            data: token.0,
            span: token.1.into(),
        })
    }

    #[inline]
    pub fn peek(&mut self) -> Option<Token> {
        let mut token = self.tokens.peek()?.to_owned();

        while token.0 == Whitespace {
            self.tokens.advance_cursor();
            token = self.tokens.peek()?.to_owned();
        }

        self.tokens.reset_cursor();

        Some(Token {
            data: token.0,
            span: token.1.clone().into(),
        })
    }

    pub fn parse(&mut self) -> TopLevel {
        let mut rodeo = Rodeo::new();
        let mut top_level = TopLevel::new();

        let now = std::time::Instant::now();

        while self.peek().is_some() {
            self.parse_userdef(&mut rodeo, &mut top_level);
        }

        let after = now.elapsed().as_micros();
        // println!("{} microseconds", after);

        top_level
    }

    #[inline]
    fn parse_userdef(&mut self, r: &mut Rodeo, tl: &mut TopLevel) {
        let vis = self.resolve_visibility();
        let next = self.next().unwrap();


        match next.data {
            Import => {
                tl.push_import(self.parse_import(r));
            }
            Const => {
                tl.push_const(self.parse_const(r, vis));
            }
            Newline => self.parse_userdef(r, tl),
            t => {
                yeet! {
                    format!("Unexpected token `{t:?}` (expected `Import` or `Const`, found `{t:?}`)."),
                    self.pos,
                    self.source,
                    Some(next),
                    ErrKind::Syntax,
                    vec![""],
                    self.file.clone()
                };
            }
        }
    }

    #[inline]
    fn parse_import(&mut self, r: &mut Rodeo) -> super::ast::Import {
        let mut out = super::ast::Import {
            path: Spur::default(),
            selected_members: super::ast::ImportMembers::None,
        };
        let peek = self.peek().unwrap();

        match peek.data {
            Identifier => {
                let id = r.get_or_intern(self.src_at(peek.span));
                out.selected_members = ImportMembers::All(id);
                self.next();
            }
            Star => {
                out.selected_members = ImportMembers::AllEscaped;
                self.next();
            }
            LSquare => out.selected_members = {
                if let Expr::Array(array) = self.parse_array(r) {
                    if !array.iter().all(|e| match *e {
                        Expr::Identifier(_) => true,
                        _ => false
                    }) {
                        yeet! {
                            format!("Unexpected exprs when parsing import."),
                            self.pos,
                            self.source,
                            None::<Token>,
                            ErrKind::Syntax,
                            vec!["Escaped imports only take identifiers: `import [member] from \"module\"`"],
                            self.file.clone()
                        };
                    }
                    ImportMembers::Escaped(array)
                } else {
                    unreachable!()
                }
            },
            _ => (),
        }

        let next = self.next();
        match next.map(|t| t.data) {
            Some(From) => (),
            Some(t) => yeet! {
                format!("Unexpected token `{t:?}` (expected `From`, found `{t:?}`)."),
                self.pos,
                self.source,
                next,
                ErrKind::Syntax,
                vec![""],
                self.file.clone()
            },
            None => yeet! {
                format!("Unexpected end of file."),
                self.pos,
                self.source,
                next,
                ErrKind::Syntax,
                vec![""],
                self.file.clone()
            },
        }

        let peek = self.peek().unwrap();
        match peek.data {
            StringLiteral => {
                let string_expr = self.parse_string(r);

                if let Expr::Str((spur, flag)) = string_expr {
                    if flag != StrFlags::None {
                        todo!("throw error")
                    }
                    self.resolve_ss();
                    out.path = spur;
                    out
                } else {
                    todo!()
                }
            }
            t => {
                println!("{:?}", t);
                todo!()
            }
        }
    }

    #[inline]
    fn parse_const(&mut self, r: &mut Rodeo, vis: StructVis) -> super::ast::Const {
        let peek = self.peek().unwrap();

        let mut con = super::ast::Const::new();
        con.set_visibility(vis);

        match peek.data {
            Identifier => {
                self.next();
                let name = self.src_at(peek.span);
                con.set_name(r.get_or_intern(name));
            }
            t => {
                let next = self.next();
                yeet! {
                    format!("Unexpected token `{t:?}` (expected `Identifier`, found `{t:?}`)."),
                    self.pos,
                    self.source,
                    next,
                    ErrKind::Syntax,
                    vec![""],
                    self.file.clone()
                }
            }
        }

        self.resolve_assign();
        con.set_value(self.parse_expr(r));

        self.resolve_ss();

        con
    }

    #[inline]
    fn parse_string(&mut self, r: &mut Rodeo) -> super::ast::Expr {
        let next = self.next().unwrap_or_else(|| {
            yeet! {
                format!("Unexpected end of file."),
                self.pos,
                self.source,
                None::<Token>,
                ErrKind::Syntax,
                vec!["Expected string input."],
                self.file.clone()
            }
        });
        let string = self.src_at(next.span);

        let mut chars = string.chars();
        chars.nth_back(0);

        let first = chars.next().unwrap();
        match first {
            '"' => {
                let mut out = String::new();
                while let Some(c) = chars.next() {
                    out.push(if c == '\\' {
                        match chars.next() {
                            Some('n') => '\n',
                            Some('r') => '\r',
                            Some('t') => '\t',
                            Some('"') => '\"',
                            Some('\'') => '\'',
                            Some('\\') => '\\',
                            Some(c) => yeet! {
                                format!("Invalid escape char '{}'", c),
                                self.pos,
                                self.source,
                                Some(next),
                                ErrKind::Syntax,
                                vec![""],
                                self.file.clone()
                            },
                            None => unreachable!(),
                        }
                    } else {
                        c
                    })
                }

                let spur = r.get_or_intern(out);
                super::ast::Expr::Str((spur, StrFlags::None))
            }
            'b' => {
                let bytes = chars
                    .map(|c| super::ast::Expr::Byte(c as u8))
                    .collect::<Vec<_>>();
                super::ast::Expr::Array(bytes)
            }
            'r' => {
                let string = chars.collect::<String>();
                let spur = r.get_or_intern(string);
                super::ast::Expr::Str((spur, StrFlags::Raw))
            }
            _ => todo!(),
        }
    }

    #[inline]
    fn parse_int(&mut self) -> super::ast::Expr {
        let next = self
            .next()
            .unwrap_or_else(|| todo!("throw unexpected EOF error"));
        let src = self.src_at(next.span);
        let pfx = src.get(..2);
        let num = src.get(2..);

        let int = match pfx {
            Some("0x") => i32::from_str_radix(num.unwrap(), 16),
            Some("0o") => i32::from_str_radix(num.unwrap(), 8),
            Some("0b") => i32::from_str_radix(num.unwrap(), 2),
            _ => src.parse(),
        }
        .unwrap();

        match self.peek().map(|t| t.data) {
            Some(Plus) => (),
            Some(Minus) => (),
            Some(Star) => (),
            Some(ForwardSlash) => (),

            _ => (),
        }

        super::ast::Expr::Int(int)
    }

    #[inline]
    fn parse_float(&mut self) -> super::ast::Expr {
        let next = self
            .next()
            .unwrap_or_else(|| todo!("throw unexpected EOF error"));
        let num: f64 = self.src_at(next.span).parse().unwrap();

        super::ast::Expr::Float(num)
    }

    #[inline]
    fn parse_atom(&mut self, r: &mut Rodeo) -> super::ast::Expr {
        let next = self
            .next()
            .unwrap_or_else(|| todo!("throw unexpected EOF error"));
        let atom = r.get_or_intern(self.src_at(next.span));

        super::ast::Expr::Atom(atom)
    }

    #[inline]
    fn parse_char(&mut self) -> super::ast::Expr {
        let next = self
            .next()
            .unwrap_or_else(|| todo!("throw unexpected EOF error"));
        let src = self.src_at(next.span);
        src.chars().next();
        src.chars().nth_back(0);
        let c = src.chars().nth(1).unwrap();
        super::ast::Expr::Char(c)
    }

    #[inline]
    fn parse_identifier(&mut self, r: &mut Rodeo) -> super::ast::Expr {
        let next = self
            .next()
            .unwrap_or_else(|| todo!("throw unexpected EOF error"));
        let ident = r.get_or_intern(self.src_at(next.span));

        super::ast::Expr::Identifier(ident)
    }

    #[inline]
    fn parse_expr(&mut self, r: &mut Rodeo) -> super::ast::Expr {
        let peek = self.peek().unwrap();

        match peek.data {
            StringLiteral => self.parse_string(r),
            Int | BinaryLiteral | OctalLiteral | HexLiteral => self.parse_int(),
            Float => self.parse_float(),
            Atom => self.parse_atom(r),
            Identifier => self.parse_identifier(r),
            LSquare => self.parse_array(r),
            LCurly => self.parse_dict(r),
            CharLiteral => self.parse_char(),
            Error => {
                self.next();
                yeet! {
                    format!("Encoundered unknown token, expected expr input."),
                    self.pos,
                    self.source,
                    Some(peek),
                    ErrKind::Syntax,
                    vec![""],
                    self.file.clone()
                }
            }
            t => {
                println!("{:?}", t);
                todo!()
            }
        }
    }

    #[inline]
    fn parse_expr_and_return_token(&mut self, r: &mut Rodeo) -> (super::ast::Expr, Token) {
        let peek = self.peek().unwrap();

        match peek.data {
            StringLiteral => (self.parse_string(r), peek),
            Int | BinaryLiteral | OctalLiteral | HexLiteral => (self.parse_int(), peek),
            Float => (self.parse_float(), peek),
            Atom => (self.parse_atom(r), peek),
            Identifier => (self.parse_identifier(r), peek),
            LSquare => (self.parse_array(r), peek),
            LCurly => (self.parse_dict(r), peek),
            Error => {
                self.next();
                yeet! {
                    format!("Encoundered unknown token, expected expr input."),
                    self.pos,
                    self.source,
                    Some(peek),
                    ErrKind::Syntax,
                    vec![""],
                    self.file.clone()
                }
            }
            t => {
                println!("{:?}", t);
                todo!()
            }
        }
    }
    
    #[inline]
    fn parse_array(&mut self, r: &mut Rodeo) -> super::ast::Expr {
        let mut out = Vec::new();
        self.next();

        loop {
            if self.peek().map(|t| t.data).contains(&RSquare) {
                self.next();
                break super::ast::Expr::Array(Vec::with_capacity(0));
            }
            out.push(self.parse_expr(r));
            if !self.resolve_closing_array() {
                continue;
            } else {
                break super::ast::Expr::Array(out);
            }
        }
    }

    #[inline]
    fn parse_dict(&mut self, r: &mut Rodeo) -> super::ast::Expr {
        let mut out = Vec::new();
        self.next();

        loop {
            if self.peek().map(|t| t.data).contains(&RCurly) {
                self.next();
                break super::ast::Expr::Dict(Vec::with_capacity(0));
            }
            let mut temp_out = (Expr::Null, Expr::Null);

            let (expr, t) = self.parse_expr_and_return_token(r);
            let data = &t.data;
            match expr {
                id @ Expr::Identifier(_) => {
                    temp_out.0 = id;
                }

                _ => yeet! {
                    format!("Unexpected token `{data:?}` while parsing dict."),
                    self.pos,
                    self.source,
                    Some(t),
                    ErrKind::Syntax,
                    vec![""],
                    self.file.clone()
                }
            }

            let next = self.next();
            match next.map(|t| t.data) {
                Some(Colon) => (),
                Some(t) => yeet! {
                    format!("Unexpected token `{t:?}`."),
                    self.pos,
                    self.source,
                    next,
                    ErrKind::Syntax,
                    vec!["Expected colon."],
                    self.file.clone()
                },
                None => yeet! {
                    "Unexpected end of file.",
                    self.pos,
                    self.source,
                    next,
                    ErrKind::Syntax,
                    vec![""],
                    self.file.clone()
                },
            }

            temp_out.1 = self.parse_expr(r);
            out.push(temp_out);

            if !self.resolve_closing_dict() {
                continue;
            } else {
                break super::ast::Expr::Dict(out);
            }
        }
    }

    fn resolve_ss(&mut self) {
        let next = self.next();

        match next.map(|t| t.data) {
            Some(SemiColon) | Some(Newline) | None => (),
            t => println!("{:?}", t),
        };
    }

    fn resolve_visibility(&mut self) -> StructVis {
        let next = self.peek();

        match next.map(|t| t.data) {
            Some(Newline) => {
                self.next();
                self.resolve_visibility()
            }
            Some(Public) => {
                self.next();
                StructVis::Public
            }
            Some(Const | FunctionPointer | Import) => StructVis::Private,
            Some(t) => {
                self.next();
                yeet! {
                    format!("Unexpected token when resolving visibility (expected `Const`, `Import`, or `FunctionPointer`, found `{t:?}`)."),
                    self.pos,
                    self.source,
                    next,
                    ErrKind::Syntax,
                    vec![""],
                    self.file.clone()
                }
            }
            None => yeet! {
                "Unexpected end of file.",
                self.pos,
                self.source,
                next,
                ErrKind::Syntax,
                vec![""],
                self.file.clone()
            },
        }
    }

    /// true => break
    ///
    /// false => continue
    fn resolve_closing_array(&mut self) -> bool {
        match self.next().map(|t| t.data) {
            Some(Comma) => false,
            Some(RSquare) => true,

            _ => todo!(),
        }
    }

    /// true => break
    ///
    /// false => continue
    fn resolve_closing_dict(&mut self) -> bool {
        match self.next().map(|t| t.data) {
            Some(Comma) => false,
            Some(RCurly) => true,

            _ => todo!(),
        }
    }

    fn resolve_assign(&mut self) {
        let next = self.next();
        match next.map(|t| t.data) {
            Some(Assign) => (),
            Some(t) => yeet! {
                format!("Unexpected token when resolving assignment (expected `=`, found `{t:?}`)."),
                self.pos,
                self.source,
                next,
                ErrKind::Syntax,
                vec![""],
                self.file.clone()
            },
            None => yeet! {
                "Unexpected end of file.",
                self.pos,
                self.source,
                next,
                ErrKind::Syntax,
                vec![""],
                self.file.clone()
            },
        }
    }

    fn src_at<R: Into<Range<usize>>>(&self, r: R) -> &str {
        &self.source[r.into()]
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<T: Debug> {
    data: T,
    span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn as_range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

#[derive(Logos, PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum Tokens {
    // Punctuation
    // #[token("λ")]
    // Lambda,
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LSquare,

    #[token("]")]
    RSquare,

    #[token("{")]
    LCurly,

    #[token("}")]
    RCurly,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("%")]
    Percent,

    #[token("&")]
    BitwiseAnd,

    #[token("|")]
    BitwiseOr,

    #[token("|>")]
    FnCombinator,

    #[token("*")]
    Star,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("/")]
    ForwardSlash,

    #[token(r"\")]
    BackwardSlash,

    #[token("=")]
    Assign,

    #[token("==")]
    Eq,

    #[token("!=")]
    Ne,

    #[token(">")]
    Gt,

    #[token("<")]
    Lt,

    #[token(">=")]
    Ge,

    #[token("<=")]
    Le,

    #[token(":")]
    Colon,

    #[token(";")]
    SemiColon,

    // Keywords
    #[token("fn")]
    FunctionPointer,

    #[token("return")]
    Return,

    #[token("do")]
    Do,

    #[token("end")]
    End,

    #[token("let")]
    Let,

    #[token("const")]
    Const,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("match")]
    Match,

    #[token("try")]
    Try,

    #[token("catch")]
    Catch,

    #[token("final")]
    Final,

    #[token("public")]
    Public,

    #[token("import")]
    Import,

    #[token("from")]
    From,

    // Literals and values
    #[regex(r"[a-zA-Z_$][a-zA-Z0-9_$]*")]
    Identifier,

    #[regex(r"[0-9][0-9_]*")]
    Int,

    #[regex(r"([0-9][0-9_]*(\.[0-9_]+))")]
    Float,

    #[regex("0b[01](_?[01]+)*")]
    BinaryLiteral,

    #[regex("0x[a-fA-F0-9](_?[a-fA-F0-9]+)*")]
    HexLiteral,

    #[regex("0o[0-7](_?[0-7]+)*")]
    OctalLiteral,

    #[regex(r#"[a-z]?"(?:\\.|[^\\"])*""#)]
    StringLiteral,

    #[regex(r"'(?:\\.|[^\\'])*'")]
    CharLiteral,

    #[regex(":[a-zA-Z_$][a-zA-Z0-9_$]*")]
    Atom,

    // Compiler extras
    #[regex(r"#!?\[(?:[a-zA-Z_][a-zA-Z_0-9]*)?\]")]
    CompilerNotes,

    // Whitespaces
    #[regex(r"[ \f\t]")]
    Whitespace,

    #[regex(r"\n")]
    Newline,

    #[error]
    Error,
}

impl<T: Debug> Spanned<T> {
    pub fn span(&self) -> Span {
        self.span
    }
}

impl<T: Debug> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {}", self.data, self.span)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}..{}): {}", self.start, self.end, self.len())
    }
}

impl ConvertFrom<Span> for Range<usize> {
    fn from(range: Span) -> Self {
        range.start..range.end
    }
}

impl ConvertFrom<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span {
            start: range.start,
            end: range.end,
        }
    }
}

impl Index<Span> for String {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[..][index.start..index.end]
    }
}
