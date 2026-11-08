//! 多模块系统测试：覆盖 编译器 → Module → VM → 宿主 的协同路径。
//!
//! 与 `test_embed.rs` 的单向嵌入示例不同，这里强调「多个模块一起工作」：
//! 编译产物复用、类型化宿主返回值、native 函数、错误分层、组合脚本、
//! 同步/异步入口一致性。
use std::sync::Arc;

use evalit::{Environment, Error, Interpreter, Module, RuntimeError, VM, Value, compile, eval};

#[cfg(feature = "async")]
use evalit::eval_async;

fn run_vm(program: Arc<Module>, env: Environment) -> Result<Value, RuntimeError> {
    let mut vm = VM::new(program, env);
    #[cfg(not(feature = "async"))]
    let ret = vm.run()?;

    #[cfg(feature = "async")]
    let ret = futures::executor::block_on(vm.run())?;

    Ok(ret.unwrap().take())
}

fn result(script: &str) -> Value {
    Interpreter::eval(script, Environment::new())
        .unwrap()
        .unwrap()
}

#[test]
fn compiler_vm_pipeline_agrees_with_interpreter() {
    let script = r#"
    fn add(a, b) {
        return a + b;
    }
    return add(20, 22);
    "#;
    let env = Environment::new();
    let module = compile(script, &env).unwrap();
    let via_vm = run_vm(module, env.clone()).unwrap();
    let via_interpreter = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(via_vm, 42i64);
    assert_eq!(via_interpreter, 42i64);
}

#[test]
fn one_module_runs_with_different_environments() {
    let script = "return factor * 3;";
    let env1 = Environment::new().with_variable("factor", 4i64);
    let env2 = Environment::new().with_variable("factor", 7i64);
    let module = compile(script, &env1).unwrap();

    assert_eq!(run_vm(module.clone(), env1).unwrap(), 12i64);
    assert_eq!(run_vm(module, env2).unwrap(), 21i64);
}

#[test]
fn module_exposes_a_consistent_snapshot() {
    let env = Environment::new();
    let module = compile(
        r#"
        fn helper(x) { return x + 1; }
        let y = helper(2);
        return y;
        "#,
        &env,
    )
    .unwrap();

    assert!(!module.instructions.is_empty());
    assert_eq!(module.symtab.len(), 1);
    for (_, &entry) in &module.symtab {
        assert!(
            entry < module.instructions.len(),
            "function entry must be inside the instruction stream"
        );
    }
    assert_eq!(run_vm(module, env).unwrap(), 3i64);
}

#[test]
fn typed_eval_returns_host_values() {
    assert_eq!(
        eval::<i64>("return 7;", Environment::new()).unwrap(),
        Some(7)
    );
    assert_eq!(
        eval::<bool>("return true;", Environment::new()).unwrap(),
        Some(true)
    );
    assert_eq!(
        eval::<String>(r#"return "hi";"#, Environment::new()).unwrap(),
        Some("hi".to_string())
    );
}

#[test]
fn typed_eval_reports_type_mismatch_as_runtime_error() {
    let err = eval::<i64>("return true;", Environment::new()).unwrap_err();
    assert!(matches!(
        err,
        Error::Runtime(RuntimeError::InvalidType { .. })
    ));
}

#[test]
fn compile_and_runtime_failures_are_distinct() {
    let bad_syntax = "let = ;";
    assert!(matches!(
        Interpreter::eval(bad_syntax, Environment::new()),
        Err(Error::Compile(_))
    ));

    let deep_recursion = r#"
    fn recurse(n) {
        if n > 0 { return recurse(n - 1); }
        return 0;
    }
    return recurse(10000);
    "#;
    assert!(matches!(
        Interpreter::eval(deep_recursion, Environment::new()),
        Err(Error::Runtime(RuntimeError::StackOverflow))
    ));
}

fn inc(x: i64) -> i64 {
    x + 1
}

#[test]
fn native_function_bridges_host_and_script() {
    let env = Environment::new().with_function("inc", inc);
    assert_eq!(eval::<i64>("return inc(41);", env).unwrap(), Some(42));
}

#[test]
fn environment_is_reusable_across_compilations() {
    let env = Environment::new().with_variable("base", 100i64);
    for delta in 0..5i64 {
        let script = format!("return base + {delta};");
        assert_eq!(
            Interpreter::eval(&script, env.clone()).unwrap().unwrap(),
            100i64 + delta
        );
    }
    // 复用不得污染环境本身。
    assert_eq!(env.keys().count(), 1);
}

#[test]
fn composed_program_exercises_parser_typing_ir_codegen_and_vm() {
    let script = r#"
    fn sum(n) {
        let total = 0;
        for i in 0..=n {
            total = total + i;
        }
        return total;
    }
    let arr = [1, 2, 3];
    let m = {"a": 10};
    m.insert("b", 20);
    let s = sum(4);
    if s != 10 { return -1; };
    if arr.len() != 3 { return -2; };
    if m["b"] != 20 { return -3; };
    return s + arr.len() + m["a"] + m["b"];
    "#;
    assert_eq!(result(script), 43i64);
}

#[test]
fn compiling_the_same_script_twice_yields_the_same_module() {
    let env = Environment::new();
    let first = compile("let a = 1 + 2; return a;", &env).unwrap();
    let second = compile("let a = 1 + 2; return a;", &env).unwrap();
    assert_eq!(format!("{first}"), format!("{second}"));
}

#[cfg(feature = "async")]
#[tokio::test]
async fn async_entry_matches_sync_entry() {
    let script = "return 6 * 7;";
    let async_ret = eval_async::<i64>(script, Environment::new()).await.unwrap();
    let sync_ret = eval::<i64>(script, Environment::new()).unwrap();
    assert_eq!(async_ret, sync_ret);
    assert_eq!(async_ret, Some(42));
}

#[test]
fn host_values_can_be_read_back_from_environment() {
    let mut env = Environment::new();
    env.insert("answer", 42i64);
    assert_eq!(env.remove_as::<i64>("answer"), Some(42));
    let env = Environment::new().with_variable("answer", 42i64);
    assert_eq!(eval::<i64>("return answer;", env).unwrap(), Some(42));
}
