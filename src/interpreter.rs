use crate::compiler::Compiler;
use crate::runtime::{Environment, VM};
use crate::{Error, Object, RuntimeError, Value};

#[cfg(feature = "async")]
pub fn eval<T: Object + 'static>(script: &str, env: Environment) -> Result<Option<T>, Error> {
    let compiler = Compiler::new();
    let module = compiler.compile(script, &env)?;

    let mut vm = VM::new(module, env);

    let ret = futures::executor::block_on(async { vm.run().await })?;

    match ret {
        Some(v) => {
            let v = v.take();
            v.into_inner::<T>()
                .map(|v| Some(v))
                .map_err(|err| RuntimeError::invalid_type::<T>(err).into())
        }
        None => Ok(None),
    }
}

#[cfg(feature = "async")]
pub async fn eval_async<T: Object + 'static>(
    script: &str,
    env: Environment,
) -> Result<Option<T>, Error> {
    let compiler = Compiler::new();
    let module = compiler.compile(script, &env)?;

    let mut vm = VM::new(module, env);

    let ret = vm.run().await?;

    match ret {
        Some(v) => {
            let v = v.take();
            v.into_inner::<T>()
                .map(|v| Some(v))
                .map_err(|err| RuntimeError::invalid_type::<T>(err).into())
        }
        None => Ok(None),
    }
}

#[cfg(not(feature = "async"))]
pub fn eval<T: Object + 'static>(script: &str, env: Environment) -> Result<Option<T>, Error> {
    let compiler = Compiler::new();
    let module = compiler.compile(script, &env)?;

    let mut vm = VM::new(module, env);

    let ret = vm.run()?;

    match ret {
        Some(v) => {
            let v = v.take();
            v.into_inner::<T>()
                .map(|v| Some(v))
                .map_err(|err| RuntimeError::invalid_type::<T>(err).into())
        }
        None => Ok(None),
    }
}

pub struct Interpreter {}

impl Interpreter {
    #[cfg(feature = "async")]
    pub fn eval(script: &str, env: Environment) -> Result<Option<Value>, Error> {
        let compiler = Compiler::new();
        let module = compiler.compile(script, &env)?;

        let mut vm = VM::new(module, env);

        let ret = futures::executor::block_on(async { vm.run().await })?;

        Ok(ret.map(|v| v.take()))
    }

    #[cfg(not(feature = "async"))]
    pub fn eval(script: &str, env: Environment) -> Result<Option<Value>, Error> {
        let compiler = Compiler::new();
        let module = compiler.compile(script, &env)?;

        let mut vm = VM::new(module, env);

        let ret = vm.run()?;

        Ok(ret.map(|v| v.take()))
    }
}
