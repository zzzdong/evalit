use crate::{ast::ParseError, compiler::CompileError, runtime::RuntimeError};

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Parse(ParseError),
    Compile(CompileError),
    Runtime(RuntimeError),
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::Io(error)
    }
}

impl From<ParseError> for Error {
    fn from(error: ParseError) -> Self {
        Error::Parse(error)
    }
}

impl From<CompileError> for Error {
    fn from(error: CompileError) -> Self {
        Error::Compile(error)
    }
}

impl From<RuntimeError> for Error {
    fn from(error: RuntimeError) -> Self {
        Error::Runtime(error)
    }
}
