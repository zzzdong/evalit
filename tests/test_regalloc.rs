//! 寄存器分配回归护栏。
//!
//! 对应 biujs 在同源分配器上踩过的四个缺陷：区间空洞、`alloc` 吞掉 spill、
//! 跨块寄存器状态不可信、phi 交接不一致。这些脚本都要求「跨块的值不丢」，
//! 单看编译成功是抓不到的。
use evalit::{Environment, Interpreter, Value};

fn run(script: &str) -> Value {
    Interpreter::eval(script, Environment::new())
        .unwrap()
        .unwrap()
}

#[test]
fn loop_with_carried_value() {
    let script = r#"
    let sum = 0;
    for i in 0..=10 {
        sum = sum + i;
    }
    return sum;
    "#;
    assert_eq!(run(script), 55i64);
}

#[test]
fn loop_with_many_live_values() {
    let script = r#"
    let a = 1;
    let b = 2;
    let c = 3;
    let d = 4;
    let sum = 0;
    for i in 0..=10 {
        a = a + i;
        b = b + i;
        c = c + i;
        d = d + i;
        sum = sum + a + b + c + d;
    }
    return sum;
    "#;

    let mut expected = 0i64;
    let (mut a, mut b, mut c, mut d) = (1i64, 2, 3, 4);
    for i in 0..=10i64 {
        a += i;
        b += i;
        c += i;
        d += i;
        expected += a + b + c + d;
    }
    assert_eq!(run(script), expected);
}

#[test]
fn nested_loops_keep_induction_variables() {
    let script = r#"
    let total = 0;
    for i in 0..=3 {
        for j in 0..=3 {
            total = total + i * j;
        }
    }
    return total;
    "#;
    assert_eq!(run(script), 36i64);
}

#[test]
fn branch_phi_inside_loop() {
    let script = r#"
    let acc = 0;
    for i in 0..=5 {
        if i % 2 == 0 {
            acc = acc + i;
        } else {
            acc = acc - i;
        };
    };
    return acc;
    "#;
    assert_eq!(run(script), -3i64);
}

#[test]
fn call_inside_loop_preserves_registers() {
    let script = r#"
    fn inc(x) {
        return x + 1;
    }
    let sum = 0;
    for i in 0..=10 {
        sum = sum + inc(i);
    }
    return sum;
    "#;
    assert_eq!(run(script), 66i64);
}

/// 变量数超过 16 个通用寄存器：修复前 `must_alloc()` 无空闲寄存器直接 panic。
#[test]
fn more_live_values_than_registers() {
    let script = r#"
    let a0 = 0;
    let a1 = 1;
    let a2 = 2;
    let a3 = 3;
    let a4 = 4;
    let a5 = 5;
    let a6 = 6;
    let a7 = 7;
    let a8 = 8;
    let a9 = 9;
    let a10 = 10;
    let a11 = 11;
    let a12 = 12;
    let a13 = 13;
    let a14 = 14;
    let a15 = 15;
    let a16 = 16;
    let a17 = 17;
    let a18 = 18;
    let a19 = 19;
    return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9
         + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19;
    "#;
    assert_eq!(run(script), 190i64);
}
