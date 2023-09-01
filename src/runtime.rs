use std::any::type_name_of_val;

use object::OperateKind;

mod environment;
mod object;
mod value;
mod vm;

use crate::bytecode::{Operand, Register};
pub use environment::Environment;
pub use object::{Callable, Enumerator, NativeFunction, Null, Object, Promise, Range};
pub use value::{Value, ValueRef};
pub use vm::VM;

pub(crate) use object::{Immd, UserFunction};

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    StackOverflow,
    InvalidRegisterAccess(Register),
    InvalidOperation {
        kind: OperateKind,
        message: String,
    },
    InvalidType {
        expected: &'static str,
        got: String,
    },
    InvalidArgumentCount {
        expected: usize,
        got: usize,
    },
    InvalidArgument {
        index: usize,
        expected: &'static str,
        cause: String,
    },
    SymbolNotFound {
        name: String,
    },
    Overflow,
    IndexOutOfBounds {
        index: i64,
        length: i64,
    },
    KeyNotFound {
        key: String,
    },
    Internal {
        message: String,
    },
    InvalidOperand {
        operand: Operand,
    },
}

impl RuntimeError {
    pub fn invalid_operation(kind: OperateKind, message: impl Into<String>) -> Self {
        RuntimeError::InvalidOperation {
            kind,
            message: message.into(),
        }
    }

    pub fn invalid_type<T: std::any::Any>(got: impl std::fmt::Debug) -> Self {
        RuntimeError::InvalidType {
            expected: std::any::type_name::<T>(),
            got: format!("type:{}, value:{:?}", type_name_of_val(&got), got),
        }
    }

    pub fn invalid_argument_count(expected: usize, got: usize) -> Self {
        RuntimeError::InvalidArgumentCount { expected, got }
    }

    pub fn invalid_argument<T: std::any::Any>(index: usize, got: impl std::fmt::Debug) -> Self {
        RuntimeError::InvalidArgument {
            index,
            expected: std::any::type_name::<T>(),
            cause: format!("{got:?}"),
        }
    }

    pub fn symbol_not_found(name: impl ToString) -> Self {
        RuntimeError::SymbolNotFound {
            name: name.to_string(),
        }
    }

    pub fn index_out_of_bound(index: i64, length: i64) -> Self {
        RuntimeError::IndexOutOfBounds { index, length }
    }

    pub fn key_not_found(key: impl std::fmt::Debug) -> Self {
        RuntimeError::KeyNotFound {
            key: format!("{key:?}"),
        }
    }

    pub fn internal(message: impl ToString) -> Self {
        RuntimeError::Internal {
            message: message.to_string(),
        }
    }

    pub fn invalid_operand(operand: Operand) -> Self {
        RuntimeError::InvalidOperand { operand }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::StackOverflow => write!(f, "Stack overflow"),
            RuntimeError::InvalidRegisterAccess(register) => {
                write!(f, "Invalid register access: {register:?}")
            }
            RuntimeError::InvalidOperation { kind, message } => {
                write!(f, "Invalid `{kind}` operation: {message}")
            }
            RuntimeError::InvalidType { expected, got } => {
                write!(
                    f,
                    "Invalid type: expected type `{expected}`, got `{got}`"
                )
            }
            RuntimeError::InvalidArgumentCount { expected, got } => {
                write!(
                    f,
                    "Invalid argument count: expected `{expected}`, got `{got}`"
                )
            }
            RuntimeError::InvalidArgument {
                index,
                expected,
                cause,
            } => {
                write!(
                    f,
                    "Invalid argument[{index}] type: expected type `{expected}`, cause `{cause}`"
                )
            }
            RuntimeError::SymbolNotFound { name } => {
                write!(f, "Symbol `{name}` not found")
            }
            RuntimeError::IndexOutOfBounds { index, length } => {
                write!(
                    f,
                    "Index out of bounds: index `{index}`, length `{length}`"
                )
            }
            RuntimeError::KeyNotFound { key } => {
                write!(f, "Key `{key}` not found")
            }
            RuntimeError::Overflow => write!(f, "Overflow"),
            RuntimeError::Internal { message } => write!(f, "Internal error: {message}"),
            RuntimeError::InvalidOperand { operand } => {
                write!(f, "Invalid operand: {operand:?}")
            }
        }
    }
}

impl std::error::Error for RuntimeError {}
