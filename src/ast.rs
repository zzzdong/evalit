mod ast;
// mod interpreter;
mod parser;

pub use ast::*;
pub use parser::{ParseError, parse_file};
