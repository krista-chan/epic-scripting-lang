use super::parser::Token;
use colored::Colorize;
use std::fmt::Debug;
use std::{error::Error, fmt::Display, path::PathBuf};

#[derive(Clone)]
pub enum ErrKind {
    Syntax,
    Runtime,
    Error,
}

impl Debug for ErrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Syntax => write!(f, "SyntaxError"),
            Self::Runtime => write!(f, "RuntimeError"),
            Self::Error => write!(f, "Error"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub(crate) message: String,
    pub(crate) pos: (usize, usize),
    pub(crate) source: String,
    pub(crate) span: Option<Token>,
    pub(crate) kind: ErrKind,
    pub(crate) notes: Vec<String>,
    pub(crate) file: Option<PathBuf>,
}

impl Default for Diagnostic {
    fn default() -> Self {
        Diagnostic {
            message: String::new(),
            kind: ErrKind::Error,
            source: String::new(),
            span: None,
            pos: (0, 0),
            notes: Vec::new(),
            file: None,
        }
    }
}

impl Diagnostic {
    fn new(kind: ErrKind) -> Self {
        Diagnostic {
            kind,
            ..Default::default()
        }
    }

    pub fn exit(self) -> ! {
        eprintln!("{}", self);
        std::process::exit(1);
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ErrKind::Syntax => {
                if let Some(token) = self.span.as_ref() {
                    let pos = (self.pos.0 + 1, self.pos.1 + 1);
                    let src = &self.source;
                    let src = format!(
                        "{}{}{}",
                        &src[..token.span().start],
                        src[token.span().as_range()]
                            .bright_cyan()
                            .underline()
                            .bold(),
                        &src[token.span().end..]
                    );
                    println!("{:?}", self.pos);
                    let line = src.lines().nth(self.pos.0).unwrap();
                    let mut file_msg = String::new();

                    if let Some(file) = &self.file {
                        file_msg = format!("in {}", file.display())
                    }

                    let notes_fmt = self
                        .notes
                        .clone()
                        .into_iter()
                        .filter_map(|n| {
                            if n.is_empty() {
                                None
                            } else {
                                Some("- ".to_owned() + &n + "\n")
                            }
                        })
                        .collect::<String>();

                    write!(
                        f,
                        "{} has occurred @ {:?} {}\n{} | {}\n{}{}  {}\n\t{}",
                        format!("{:?}", self.kind).green(),
                        pos,
                        file_msg,
                        pos.0.to_string().yellow(),
                        line,
                        " ".repeat(pos.1 + (pos.0.to_string().len()) + 1),
                        "╰⇢".bright_cyan().bold(),
                        self.message.bright_red(),
                        notes_fmt.trim_end().bright_cyan()
                    )
                } else {
                    let pos = (self.pos.0 + 1, self.pos.1 + 1);
                    let file_msg = self
                        .file
                        .as_ref()
                        .map(|p| p.to_str().unwrap())
                        .unwrap_or("repl");

                    let notes_fmt = self
                        .notes
                        .clone()
                        .into_iter()
                        .filter_map(|n| {
                            if n.is_empty() {
                                None
                            } else {
                                Some("- ".to_owned() + &n + "\n")
                            }
                        })
                        .collect::<String>();

                    write!(
                        f,
                        "{} has occurred @ {:?} in {}\n\t{}\n\t{}",
                        format!("{:?}", self.kind).green(),
                        pos,
                        file_msg,
                        self.message.bright_red(),
                        notes_fmt.trim_end().bright_cyan()
                    )
                }
            }

            _ => todo!(),
        }
    }
}

impl Error for Diagnostic {}
