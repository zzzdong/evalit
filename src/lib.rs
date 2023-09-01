mod ast;
mod bytecode;
mod compiler;
mod error;
mod interpreter;
mod ir;
mod runtime;

#[cfg(test)]
mod test_utils;

pub use error::Error;
pub use interpreter::Interpreter;
pub use runtime::{Environment, NativeFunction, Null, Object, Promise, RuntimeError, ValueRef, Value};
// #[cfg(test)]
// pub use test_utils::assert_value_eq;
