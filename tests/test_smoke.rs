//! 修改验证用的冒烟测试：一条命令跑完最小功能面 + 常见输入边界。
//!
//!     cargo test --test test_smoke
//!
//! 这里刻意只做浅层断言：目标是「改动后基本功能还在」，不是细粒度语义验证；
//! 深度正确性由 `test_correctness.rs` 与各特性测试负责。
use evalit::{Environment, Error, Interpreter, Null, Value};

enum Expect {
    Int(i64),
    Bool(bool),
    Str(&'static str),
    Null,
}

struct Case {
    name: &'static str,
    script: &'static str,
    expect: Expect,
}

fn assert_expect(name: &str, value: Value, expect: &Expect) {
    match expect {
        Expect::Int(expected) => assert_eq!(value, *expected, "smoke case `{name}`"),
        Expect::Bool(expected) => assert_eq!(value, *expected, "smoke case `{name}`"),
        Expect::Str(expected) => assert_eq!(value, *expected, "smoke case `{name}`"),
        Expect::Null => assert_eq!(value, Null, "smoke case `{name}`"),
    }
}

fn cases() -> Vec<Case> {
    vec![
        Case {
            name: "arithmetic_precedence",
            script: "return 1 + 2 * 3;",
            expect: Expect::Int(7),
        },
        Case {
            name: "integer_division",
            script: "return 10 / 2;",
            expect: Expect::Int(5),
        },
        Case {
            name: "unary_negation",
            script: "return -3 + 5;",
            expect: Expect::Int(2),
        },
        Case {
            name: "variables_and_reassignment",
            script: "let a = 5; a += 2; return a;",
            expect: Expect::Int(7),
        },
        Case {
            name: "if_else",
            script: "if 2 > 1 { return 10; } else { return 20; }",
            expect: Expect::Int(10),
        },
        Case {
            name: "for_loop",
            script: "let s = 0; for i in 0..=5 { s += i; } return s;",
            expect: Expect::Int(15),
        },
        Case {
            name: "loop_with_break",
            script: "let i = 0; loop { i += 1; if i == 3 { break; }; }; return i;",
            expect: Expect::Int(3),
        },
        Case {
            name: "function_call",
            script: "fn add(a, b) { return a + b; } return add(3, 4);",
            expect: Expect::Int(7),
        },
        Case {
            name: "recursion",
            script: "fn fact(n) { if n <= 1 { return 1; } return n * fact(n - 1); } return fact(5);",
            expect: Expect::Int(120),
        },
        Case {
            name: "boolean_expression",
            script: "return true && !false;",
            expect: Expect::Bool(true),
        },
        Case {
            name: "string_literal",
            script: r#"return "abc";"#,
            expect: Expect::Str("abc"),
        },
        Case {
            name: "string_equality",
            script: r#"return "hi" == "hi";"#,
            expect: Expect::Bool(true),
        },
        Case {
            name: "array_index",
            script: "let arr = [1, 2, 3]; return arr[1];",
            expect: Expect::Int(2),
        },
        Case {
            name: "map_lookup",
            script: r#"let m = {"a": 5}; return m["a"];"#,
            expect: Expect::Int(5),
        },
        Case {
            name: "array_method",
            script: "let arr = [1, 2, 3]; return arr.len();",
            expect: Expect::Int(3),
        },
        Case {
            name: "no_explicit_return",
            script: "let x = 1; x + 1;",
            expect: Expect::Null,
        },
    ]
}

#[test]
fn smoke_suite() {
    for case in cases() {
        let value = Interpreter::eval(case.script, Environment::new())
            .unwrap_or_else(|err| panic!("smoke case `{}` failed to run: {err:?}", case.name))
            .unwrap_or_else(|| panic!("smoke case `{}` produced no value", case.name));
        assert_expect(case.name, value, &case.expect);
    }
}

#[test]
fn empty_and_whitespace_only_scripts_are_valid() {
    for script in ["", "   ", "\r\n\t\r\n", "// just a comment\n"] {
        assert!(
            Interpreter::eval(script, Environment::new()).is_ok(),
            "empty-ish script must not fail: {script:?}"
        );
    }
}

#[test]
fn crlf_script_with_comment_still_evaluates() {
    let script = "\r\n// header comment\r\nlet a = 3;\r\nreturn a * 2;\r\n";
    let value = Interpreter::eval(script, Environment::new())
        .unwrap()
        .unwrap();
    assert_eq!(value, 6i64);
}

#[test]
fn invalid_syntax_is_reported_as_a_compile_error() {
    assert!(matches!(
        Interpreter::eval("let = ;", Environment::new()),
        Err(Error::Compile(_))
    ));
}

#[test]
fn host_function_smoke() {
    fn identity(x: i64) -> i64 {
        x
    }
    let env = Environment::new().with_function("identity", identity);
    let value = Interpreter::eval("return identity(9);", env)
        .unwrap()
        .unwrap();
    assert_eq!(value, 9i64);
}

#[test]
fn repeated_evaluation_is_stable() {
    let script = "let a = 2; return a * 21;";
    for _ in 0..5 {
        let value = Interpreter::eval(script, Environment::new())
            .unwrap()
            .unwrap();
        assert_eq!(value, 42i64);
    }
}

/// grammar 允许、但实现尚未覆盖（或含非法字面量）的输入必须得到编译错误，
/// 不能在解析器 / 语义分析 / lowering 里 panic。
#[test]
fn invalid_scripts_never_panic() {
    const BAD_SCRIPTS: &[&str] = &[
        "中",                               // 非 ASCII 起始 token
        "let 中 = 1;",                      // 错误 span 落在 UTF-8 字符内部
        "return 999999999999999999999999;", // 整数字面量溢出
        "return -999999999999999999999999;",
        "return 1 + 999999999999999999999999;",
        "1 += 999999999999999999999999;",
        "return 1 ^ 2;",                     // grammar 有、BinOp 没有
        "return 1 as 1;",                    // AST 有、lowering 没有
        "return self::foo;",                 // path 表达式
        "return (1, 2);",                    // tuple 表达式
        "1 = 2;",                            // 非法左值
        "let a = [1]; a[zz] = 2;",           // index 位置的未定义标识符
        "let f = |x| { return zz; };",       // 闭包体未分析
        "let f = |x| { return NoSuch{}; };", // 闭包体里的未定义结构体
        "for 1 in [1] {}",                   // literal pattern
        "enum E {}",                         // 空 enum 声明
    ];

    for script in BAD_SCRIPTS {
        assert!(
            matches!(
                Interpreter::eval(script, Environment::new()),
                Err(Error::Compile(_))
            ),
            "expected a compile error, got a panic or a value: {script:?}"
        );
    }
}

#[test]
fn compile_errors_report_real_line_column_and_utf8_span() {
    let err = match Interpreter::eval("let a = 1;\nlet b = ;", Environment::new()) {
        Err(Error::Compile(err)) => err,
        other => panic!("expected compile error, got {other:?}"),
    };
    let text = err.to_string();
    assert!(text.contains("line: 2"), "{text}");
    assert!(text.contains("col: 9"), "{text}");

    // 旧实现在 `&input[pos..pos+1]` 上切多字节字符会直接 panic。
    let err = match Interpreter::eval("let 中 = 1;", Environment::new()) {
        Err(Error::Compile(err)) => err,
        other => panic!("expected compile error, got {other:?}"),
    };
    let text = err.to_string();
    assert!(text.contains("Error on 中"), "{text}");
    assert!(text.contains("line: 1"), "{text}");
    assert!(text.contains("col: 5"), "{text}");
}
