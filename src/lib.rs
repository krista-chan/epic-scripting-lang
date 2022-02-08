#![allow(dead_code)]
#![feature(option_result_contains)]

pub mod parser;

#[cfg(test)]
pub mod tests;

use parser::ast::TopLevel;
use parser::error::Diagnostic;

#[macro_export]
macro_rules! error {
    ($message:expr, $pos:expr, $source:expr, $token:expr, $kind:expr, $notes:expr, $file:expr) => {
        $crate::Diagnostic {
            message: $message.to_string(),
            pos: $pos,
            source: $source.to_string(),
            span: $token,
            kind: $kind,
            notes: $notes.into_iter().map(|n| n.to_string()).collect(),
            file: $file,
        }
    };
}

#[macro_export]
macro_rules! yeet {
    ($message:expr, $pos:expr, $source:expr, $token:expr, $kind:expr, $notes:expr, $file:expr) => {
        $crate::error! {
            $message, $pos, $source, $token, $kind, $notes, $file
        }
        .exit()
    };
}
