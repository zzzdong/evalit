//! 单栈 + 显式帧的回归护栏。
//!
//! 删除 ctrl_stack / PushC / PopC / MovC 之后，返回地址与帧基址由 VM 的
//! Frame 管理：`rbp` 只在开帧写一次，退帧时由帧派生。这些用例专门验证
//! 「被调函数不能碰坏调用方的局部槽」以及实参顺序。
use evalit::{Environment, Interpreter, Value};

fn run(script: &str) -> Value {
    Interpreter::eval(script, Environment::new())
        .unwrap()
        .unwrap()
}

#[test]
fn called_function_does_not_clobber_caller_locals() {
    let script = r#"
    fn inner(x) {
        let a = x + 1;
        let b = x + 2;
        return a + b;
    }
    let v0 = 10;
    let v1 = 20;
    let r = inner(5);
    return v0 + v1 + r;
    "#;
    assert_eq!(run(script), 43i64);
}

#[test]
fn nested_calls_restore_frame_base() {
    let script = r#"
    fn leaf(x) {
        return x + 1;
    }
    fn middle(x) {
        let a = 10;
        let b = leaf(x) + a;
        let c = leaf(b) + a;
        return b + c;
    }
    let keep = 7;
    let r = middle(5);
    return keep + r;
    "#;
    assert_eq!(run(script), 50i64);
}

#[test]
fn recursion_restores_each_frame() {
    let script = r#"
    fn factorial(n) {
        if n <= 1 {
            return 1;
        }
        return n * factorial(n - 1);
    }
    return factorial(6);
    "#;
    assert_eq!(run(script), 720i64);
}

#[test]
fn call_with_multiple_arguments_keeps_order() {
    let script = r#"
    fn add3(a, b, c) {
        return a + b + c;
    }
    let total = 0;
    for i in 0..=3 {
        total = total + add3(i, i + 1, i + 2);
    }
    return total;
    "#;
    assert_eq!(run(script), 30i64);
}

/// 原生/方法调用不再改 rbp，实参按 rsp 相对读取；调用方局部槽必须完好。
#[test]
fn method_calls_keep_caller_stack_slots() {
    let script = r#"
    let m = {"a": 1};
    m.insert("b", 2);
    let marker = 100;
    if m.len() != 2 {
        return -1;
    };
    return marker + m["b"];
    "#;
    assert_eq!(run(script), 102i64);
}
