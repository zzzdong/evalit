mod codegen;
mod lowering;
mod regalloc;
mod semantic;

use std::collections::HashMap;

use log::debug;

use crate::ast::{ParseError, Span, Type};
use crate::bytecode::{Module, Register};
use crate::{Environment, Error};

use codegen::Codegen;
use lowering::lowering;
use semantic::SemanticAnalyzer;

pub fn compile(script: &str, env: &crate::Environment) -> Result<crate::Module, crate::Error> {
    Compiler::new().compile(script, env)
}

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    Semantics(String),
    UndefinedVariable {
        name: String,
        span: Span,
    },
    TypeMismatch {
        expected: Type,
        actual: Type,
        span: Span,
    },
    TypeInference(String),
    TypeCheck(String),
    ArgumentCountMismatch {
        expected: usize,
        actual: usize,
        span: Span,
    },
    NotCallable {
        ty: Type,
        span: Span,
    },
    Unreachable,
    BreakOutsideLoop {
        span: Span,
    },
    ContinueOutsideLoop {
        span: Span,
    },
    ReturnOutsideFunction {
        span: Span,
    },
}

impl From<ParseError> for CompileError {
    fn from(error: ParseError) -> Self {
        CompileError::Parse(error)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Parse(error) => write!(f, "Parse error: {error}"),
            CompileError::Semantics(message) => write!(f, "Semantics error: {message}"),
            CompileError::UndefinedVariable { name, span } => {
                write!(f, "Undefined variable `{name}` at {span:?}")
            }
            CompileError::TypeMismatch {
                expected,
                actual,
                span,
            } => write!(
                f,
                "Type mismatch: expected `{expected:?}`, actual `{actual:?}` at {span:?}"
            ),
            CompileError::TypeInference(message) => write!(f, "Type inference error: {message}"),
            CompileError::TypeCheck(message) => write!(f, "Type check error: {message}"),
            CompileError::ArgumentCountMismatch {
                expected,
                actual,
                span,
            } => write!(
                f,
                "Argument count mismatch: expected {expected}, actual {actual} at {span:?}"
            ),
            CompileError::NotCallable { ty, span } => {
                write!(f, "Not callable: `{ty:?}` at {span:?}")
            }
            CompileError::Unreachable => write!(f, "Unreachable"),
            CompileError::BreakOutsideLoop { span } => write!(f, "Break outside loop at {span:?}"),
            CompileError::ContinueOutsideLoop { span } => {
                write!(f, "Continue outside loop at {span:?}")
            }
            CompileError::ReturnOutsideFunction { span } => {
                write!(f, "Return outside function at {span:?}")
            }
        }
    }
}

impl std::error::Error for CompileError {}

pub struct Compiler {}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, input: &str, env: &Environment) -> Result<Module, Error> {
        // 解析输入
        let mut ast = crate::ast::parse_file(input)?;

        debug!("AST: {ast:?}");

        // // 语义分析
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze_program(&mut ast, env)?;

        // IR生成
        let unit = lowering(ast, env)?;

        let mut codegen = Codegen::new(&Register::general());
        let insts = codegen.generate_code(unit.control_flow_graph);

        let mut instructions = insts.to_vec();

        let mut symtab = HashMap::new();

        let mut offset = instructions.len();
        for func in unit.functions {
            let mut codegen = Codegen::new(&Register::general());
            let insts = codegen.generate_code(func.control_flow_graph);
            symtab.insert(func.id, offset);
            offset += insts.len();
            instructions.extend(insts.to_vec());
        }

        let module = Module {
            name: None,
            constants: unit.constants,
            symtab,
            instructions: instructions.to_vec(),
        };

        Ok(module)
    }
}
