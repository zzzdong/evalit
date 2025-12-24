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
use std::sync::Arc;

use ir::IrUnit;
use ir::builder::{InstBuilder, IrBuilder};
use log::debug;
use typing::{TypeChecker, TypeContext, TypeError};

use crate::Environment;
use crate::bytecode::{Module, Register};
use crate::compiler::ast::syntax::Span;
use crate::compiler::symbol::SymbolTable;
use parser::ParseError;

use codegen::Codegen;
use lowering::ASTLower;
use semantic::{SemanticAnalyzer, SemanticError};

pub(crate) use ir::Instruction;

pub fn compile<'i>(
    script: &'i str,
    env: &crate::Environment,
) -> Result<Arc<crate::Module>, CompileError<'i>> {
    Compiler::new().compile(script, env)
}

#[derive(Debug, Clone, Copy)]
struct LineCol {
    line: usize,
    col: usize,
}

impl From<(usize, usize)> for LineCol {
    fn from(value: (usize, usize)) -> Self {
        Self {
            line: value.0,
            col: value.1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompileError<'i> {
    kind: ErrorKind,
    span: &'i str,
    line_col: LineCol,
}

impl<'i> CompileError<'i> {
    pub fn new(input: &'i str, error: ErrorKind) -> Self {
        let (line_col, span) = match error.line_col(input) {
            Some(line_col) => {
                let span = error.span();
                (line_col, &input[span.start..span.end])
            }
            None => (LineCol { line: 0, col: 0 }, &input[0..0]),
        };
        Self {
            kind: error,
            span,
            line_col,
        }
    }
}

impl<'i> std::fmt::Display for CompileError<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error on {}@{:?}, detail: {:?}",
            self.span, self.line_col, self.kind
        )
    }
}

impl<'i> std::error::Error for CompileError<'i> {}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    // Io(std::io::Error),
    Parse(ParseError),
    Type(TypeError),
    Semantic(SemanticError),
}

impl ErrorKind {
    fn line_col(&self, input: &str) -> Option<LineCol> {
        match self {
            ErrorKind::Parse(ParseError { span, .. }) => span.line_col(input).map(Into::into),
            ErrorKind::Type(TypeError { span, .. }) => span.line_col(input).map(Into::into),
            ErrorKind::Semantic(SemanticError { span, .. }) => span.line_col(input).map(Into::into),
        }
    }

    fn span(&self) -> Span {
        match self {
            ErrorKind::Parse(ParseError { span, .. }) => *span,
            ErrorKind::Type(TypeError { span, .. }) => *span,
            ErrorKind::Semantic(SemanticError { span, .. }) => *span,
        }
    }
}

impl From<ParseError> for ErrorKind {
    fn from(error: ParseError) -> Self {
        ErrorKind::Parse(error)
    }
}

impl From<TypeError> for ErrorKind {
    fn from(error: TypeError) -> Self {
        ErrorKind::Type(error)
    }
}

impl From<SemanticError> for ErrorKind {
    fn from(error: SemanticError) -> Self {
        ErrorKind::Semantic(error)
    }
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Parse(error) => write!(f, "Parse error: {error}"),
            ErrorKind::Type(error) => write!(f, "Type error: {error:?}"),
            ErrorKind::Semantic(error) => write!(f, "Semantic error: {error:?}"),
        }
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

    pub fn compile<'i>(
        &self,
        input: &'i str,
        env: &Environment,
    ) -> Result<Arc<Module>, CompileError<'i>> {
        match self.compile_inner(input, env) {
            Ok(module) => Ok(module),
            Err(err) => Err(CompileError::new(input, err)),
        }
    }

    fn compile_inner(&self, input: &str, env: &Environment) -> Result<Arc<Module>, ErrorKind> {
        // 解析输入
        let ast = parser::parse_file(input)?;

        debug!("AST: {ast:?}");

        let mut type_cx = TypeContext::new();
        type_cx.check_type_def(&ast.stmts)?;

        // 语义分析
        let mut analyzer = SemanticAnalyzer::new(&type_cx);
        analyzer.analyze_program(&ast, env)?;

        // 类型检查
        let mut checker = TypeChecker::new(&type_cx);
        checker.check_program(&ast, env)?;

        // IR生成, AST -> IR
        let mut unit = IrUnit::new();
        let builder: &mut dyn InstBuilder = &mut IrBuilder::new(&mut unit);
        let mut lower = ASTLower::new(builder, SymbolTable::new(), env, &type_cx);
        lower.lower_program(ast);

        // println!("before SSA:\n{}", unit.control_flow_graph);

        // TODO: 暂时禁用SSA转换，待修复后再启用
        let mut ssa_builder = ir::SSABuilder::new(&mut unit.control_flow_graph);
        ssa_builder.convert_to_ssa();

        // println!("after SSA:\n{}", unit.control_flow_graph);

        // panic!("debug");

        // code generation, IR -> bytecode
        let mut codegen = Codegen::new(&Register::general());
        let insts = codegen.generate_code(unit.control_flow_graph);

        let mut instructions = insts.to_vec();
        let mut debug_instructions = codegen.debug_insts().clone();

        let mut symtab = HashMap::new();
        // relocate symbol table
        let mut offset = instructions.len();
        for mut func in unit.functions.into_iter() {
            let mut ssa_builder = ir::SSABuilder::new(&mut func.control_flow_graph);
            ssa_builder.convert_to_ssa();

            let mut codegen = Codegen::new(&Register::general());
            let insts = codegen.generate_code(func.control_flow_graph);
            symtab.insert(func.id, offset);
            offset += insts.len();
            instructions.extend(insts.to_vec());
            debug_instructions.extend(
                codegen
                    .debug_insts()
                    .iter()
                    .map(|(id, inst)| (*id + offset, inst.clone())),
            );
        }

        let module = Module {
            name: None,
            constants: unit.constants,
            symtab,
            instructions: instructions.to_vec(),
            debug_instructions,
        };

        Ok(Arc::new(module))
    }
}
