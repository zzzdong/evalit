mod ast;
mod codegen;
mod ir;
mod lowering;
mod parser;
mod regalloc;
mod semantic;
mod symbol;
mod typing;

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use ir::builder::{InstBuilder, IrBuilder};
use ir::instruction::IrUnit;
use log::debug;
use typing::{Type, TypeChecker, TypeContext, TypeError};

use crate::Environment;
use crate::bytecode::{Module, Register};
use parser::ParseError;

use codegen::Codegen;
use lowering::{ASTLower, SymbolTable};
use semantic::SemanticAnalyzer;

pub fn compile(script: &str, env: &crate::Environment) -> Result<Arc<crate::Module>, CompileError> {
    Compiler::new().compile(script, env)
}

#[derive(Debug)]
pub enum CompileError {
    Io(std::io::Error),
    Parse(ParseError),
    Type(TypeError),
    Semantics(String),
    UndefinedVariable {
        name: String,
    },
    UnknownType {
        name: String,
    },
    TypeMismatch {
        expected: Box<Type>,
        actual: Box<Type>,
        span: Span,
    },
    TypeInference(String),
    TypeCheck(String),
    ArgumentCountMismatch {
        expected: usize,
        actual: usize,
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
    InvalidOperation {
        message: String,
    },
}

impl CompileError {
    pub fn type_mismatch(expected: Type, actual: Type, span: Span) -> Self {
        CompileError::TypeMismatch {
            span,
            expected: Box::new(expected),
            actual: Box::new(actual),
        }
    }
}

impl From<std::io::Error> for CompileError {
    fn from(error: std::io::Error) -> Self {
        CompileError::Io(error)
    }
}

impl From<ParseError> for CompileError {
    fn from(error: ParseError) -> Self {
        CompileError::Parse(error)
    }
}

impl From<TypeError> for CompileError {
    fn from(error: TypeError) -> Self {
        CompileError::Type(error)
    }
}

impl From<SemanticsError> for CompileError {
    fn from(error: SemanticsError) -> Self {
        CompileError::Semantics(error)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Io(error) => write!(f, "IO error: {error}"),
            CompileError::Parse(error) => write!(f, "Parse error: {error}"),
            CompileError::Type(error) => write!(f, "Type error: {error:?}"),
            CompileError::Semantics(message) => write!(f, "Semantics error: {message}"),
            CompileError::UndefinedVariable { name } => {
                write!(f, "Undefined variable `{name}`")
            }
            CompileError::UnknownType { name } => {
                write!(f, "Unknow type `{name}`")
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
            CompileError::ArgumentCountMismatch { expected, actual } => write!(
                f,
                "Argument count mismatch: expected {expected}, actual {actual}"
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
            CompileError::InvalidOperation { message } => {
                write!(f, "Invalid operation, {message}")
            }
        }
    }
}

impl std::error::Error for CompileError {}

pub struct FileId(usize);

pub struct Context {
    sources: Vec<String>,
}

impl Context {
    pub fn new() -> Self {
        Self { sources: vec![] }
    }

    pub fn add_source(&mut self, source: String) -> FileId {
        let id = FileId(self.sources.len());
        self.sources.push(source);
        id
    }

    pub fn add_file(&mut self, file: impl AsRef<Path>) -> Result<FileId, CompileError> {
        let id = FileId(self.sources.len());
        let content = std::fs::read_to_string(file.as_ref())?;
        self.sources.push(content);
        Ok(id)
    }

    pub fn get_source(&self, file: FileId) -> Option<&str> {
        self.sources.get(file.0).map(|s| s.as_str())
    }
}

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

    pub fn compile(&self, input: &str, env: &Environment) -> Result<Arc<Module>, CompileError> {
        // 解析输入
        let mut ast = parser::parse_file(input)?;

        debug!("AST: {ast:?}");

        let mut type_cx = TypeContext::new();
        type_cx.analyze_type_def(&ast.stmts)?;

        // 语义分析
        let mut checker = SemanticChecker::new(&mut type_cx);
        checker.check_program(&mut ast, env)?;

        // IR生成, AST -> IR
        let mut unit = IrUnit::new();
        let builder: &mut dyn InstBuilder = &mut IrBuilder::new(&mut unit);
        let mut lower = ASTLower::new(builder, SymbolTable::new(), env, &type_cx);
        lower.lower_program(ast)?;

        // code generation, IR -> bytecode
        let mut codegen = Codegen::new(&Register::general());
        let insts = codegen.generate_code(unit.control_flow_graph);

        let mut instructions = insts.to_vec();

        let mut symtab = HashMap::new();
        // relocate symbol table
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

        Ok(Arc::new(module))
    }
}
