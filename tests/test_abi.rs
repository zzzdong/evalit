//! 调用约定回归：返回值走通用寄存器 R0（类比 x86-64 RAX / AArch64 x0），
//! 不再有专用 Rv 伪寄存器；R0 仍参与普通分配，调用点按 caller-clobbered 处理。
use evalit::{Environment, Interpreter, Null, Value};

fn run(script: &str) -> Value {
    Interpreter::eval(script, Environment::new())
        .unwrap()
        .unwrap()
}

#[test]
fn return_value_uses_a_general_register() {
    let script = r#"
    fn add(a, b) {
        return a + b;
    }
    let keep = 100;
    let r = add(3, 4);
    return keep + r;
    "#;
    assert_eq!(run(script), 107i64);
}

#[test]
fn function_without_return_yields_null() {
    let script = r#"
    fn noop() {
        let x = 1;
    }
    let r = noop();
    return r;
    "#;
    assert_eq!(run(script), Null);
}

#[test]
fn value_live_in_return_register_survives_calls() {
    let script = r#"
    fn add(a, b) {
        return a + b;
    }
    let a = 1;
    let b = 2;
    let c = 3;
    let d = 4;
    let first = add(a, b);
    let second = add(c, d);
    return a + b + c + d + first + second;
    "#;
    assert_eq!(run(script), 20i64);
}

#[test]
fn script_without_return_yields_null() {
    let script = r#"
    let a = 1;
    a = a + 1;
    "#;
    assert_eq!(run(script), Null);
}
