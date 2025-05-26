mod bytecode;
mod compiler;
mod error;
mod interpreter;
mod runtime;

pub use bytecode::Module;
pub use compiler::{Compiler, compile};
pub use error::Error;
#[cfg(feature = "async")]
pub use interpreter::eval_async;
pub use interpreter::{Interpreter, eval};

#[cfg(feature = "async")]
pub use runtime::Promise;
pub use runtime::{Environment, NativeFunction, Null, Object, RuntimeError, VM, Value, ValueRef};
