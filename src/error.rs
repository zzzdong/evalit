use crate::{compiler::CompileError, runtime::RuntimeError};

#[derive(Debug)]
pub enum Error {
    Compile(CompileError),
    Runtime(RuntimeError),
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
