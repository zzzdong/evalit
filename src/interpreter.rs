use crate::compiler::Compiler;
use crate::runtime::{Environment, Program, VM};
use crate::{Error, Value};

pub struct Interpreter {}

impl Interpreter {
    pub fn eval_script(script: &str, env: Environment) -> Result<Option<Value>, Error> {
        let compiler = Compiler::new();
        let module = compiler.compile(script, &env)?;

        let mut vm = VM::new(&module, &env);

        #[cfg(feature = "async")]
        let ret = futures::executor::block_on(async { vm.run().await })?;
        #[cfg(not(feature = "async"))]
        let ret = vm.run()?;

        Ok(ret.map(|v| v.take()))
    }
}
