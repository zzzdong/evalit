mod parser;
mod syntax;
// mod interpreter;

pub use parser::{ParseError, parse_file};
pub use syntax::*;
