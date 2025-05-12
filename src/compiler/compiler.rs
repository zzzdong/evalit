use log::debug;

use crate::bytecode::{Module, Register};
use crate::{Environment, Error, ast::*};

use super::codegen::Codegen;
use super::lowering::lowering;
use super::semantic::SemanticAnalyzer;

use std::collections::HashMap;

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

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, input: &str, env: &Environment) -> Result<Module, Error> {
        // 解析输入
        let mut ast = parse_file(input)?;

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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_compile_if_else() {
        let _ = env_logger::builder()
            .filter_level(log::LevelFilter::Trace)
            .is_test(true)
            .try_init();

        let input = r#"
        fn add(a, b) {
            let c = 1;
            let d = 2;
            let e = 3;
            let f = c * d + e;
            let g = 4;
            
            // 增加更多局部变量
            let h = 5;
            let i = 6;
            let j = 7;
            let k = 8;
            let l = 9;
            let m = 10;
            let n = 11;
            let o = 12;
            let p = 13;
            let q = 14;
            let r = 15;
            let s = 16;
            let t = 17;
            let u = 18;
            let v = 19;
            let w = 20;
            let x = 21;
            let y = 22;
            let z = 23;

            // 在不同作用域中使用局部变量
            if a > 0 {
                let inner_a = a + h;
                let inner_b = b + i;
                return inner_a + inner_b;
            } else {
                let inner_c = a - j;
                let inner_d = b - k;
                return inner_c + inner_d;
            }
        }
        return add(1, 2);
        "#;

        let compiler = Compiler::new();
        let insts = compiler.compile(input, &Environment::new()).unwrap();
        // for func in insts.functions {
        //     println!("=== Function({}) ===", func.id.as_usize());
        //     for inst in func.instructions {
        //         println!("{inst}");
        //     }
        //     println!("=== End ===");
        // }

        println!("=== Global ===");

        for inst in insts.instructions {
            println!("{inst}");
        }
    }
}
