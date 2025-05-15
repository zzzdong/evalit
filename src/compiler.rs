mod codegen;
mod compiler;
mod lowering;
mod semantic;

pub use compiler::CompileError;
pub use compiler::Compiler;

pub fn compile(script: &str, env: &crate::Environment) -> Result<crate::Module, crate::Error> {
    Compiler::new().compile(script, env)
}
