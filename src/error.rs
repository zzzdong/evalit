use crate::{compiler::CompileError, runtime::RuntimeError};

#[derive(Debug)]
pub enum Error<'i> {
    Compile(CompileError<'i>),
    Runtime(RuntimeError),
}

impl<'i> From<CompileError<'i>> for Error<'i> {
    fn from(error: CompileError<'i>) -> Self {
        Error::Compile(error)
    }
}

impl From<RuntimeError> for Error<'_> {
    fn from(error: RuntimeError) -> Self {
        Error::Runtime(error)
    }
}
