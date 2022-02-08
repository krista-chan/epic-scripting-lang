use super::parser::{Lexer, Spanned};
use lasso::Spur;
use std::fmt::Debug;
use std::str::FromStr;

pub struct AstNode<T: Debug> {
    spanned: Spanned<T>,
    attribute: Option<Attribute>,
}

#[derive(Debug)]
pub struct TopLevel {
    consts: Vec<Const>,
    imports: Vec<Import>,
}

impl TopLevel {
    pub fn new() -> Self {
        TopLevel {
            consts: Vec::new(),
            imports: Vec::new(),
        }
    }

    pub fn push_import(&mut self, import: Import) {
        self.imports.push(import);
    }

    pub fn push_const(&mut self, c: Const) {
        self.consts.push(c);
    }
}

#[derive(Debug)]
pub enum ImportMembers {
    All(Spur),
    Escaped(Vec<Expr>),
    AllEscaped,
    None,
}

#[derive(Debug)]
pub enum Expr {
    Int(i32),
    Float(f64),
    Byte(u8),
    Str((Spur, StrFlags)),
    Char(char),
    Atom(Spur),
    Bool(bool),
    Identifier(Spur),
    Array(Vec<Expr>),
    Dict(Vec<(Expr, Expr)>),
    Add { lhs: Box<Expr>, rhs: Box<Expr> },
    Sub { lhs: Box<Expr>, rhs: Box<Expr> },
    Mul { lhs: Box<Expr>, rhs: Box<Expr> },
    Div { lhs: Box<Expr>, rhs: Box<Expr> },
    Null,
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum StrFlags {
    Byte,
    Raw,
    None,
}

pub enum Types {
    Int,
    Float,
    Str,
    Char,
    Atom,
    Bool,
}

#[derive(Debug)]
pub struct Import {
    pub path: Spur,
    pub selected_members: ImportMembers,
}

#[derive(Debug)]
pub struct Const {
    name: Spur,
    value: Expr,
    visibility: StructVis,
}

impl Const {
    pub fn new() -> Self {
        Const {
            name: Spur::default(),
            value: Expr::Null,
            visibility: StructVis::Private,
        }
    }

    pub fn set_name(&mut self, name: Spur) {
        self.name = name;
    }

    pub fn set_value(&mut self, value: Expr) {
        self.value = value;
    }

    pub fn set_visibility(&mut self, visibility: StructVis) {
        self.visibility = visibility;
    }
}

pub struct Attribute {
    contents: Vec<Spur>,
    visibility: AttrVis,
}

#[derive(Debug, Clone, Copy)]
pub enum StructVis {
    Private,
    Public,
}

#[derive(Debug)]
pub enum AttrVis {
    Module,
    Statement,
}

impl FromStr for TopLevel {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lexer = Lexer::new(s, None);
        Ok(lexer.parse())
    }
}
