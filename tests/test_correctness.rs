//! 实现正确性检测套件。
//!
//! 与冒烟测试不同，这里检查「实现本身是否满足不变量」：
//! 1. 编译产物的结构不变量（跳转目标、函数入口、终止指令、常量池去重）；
//! 2. 编译确定性（重复编译逐字节一致）；
//! 3. 与独立参考实现对照（参数化的循环、递归、嵌套、算术表达式）；
//! 4. 错误分层与输入边界。
use std::collections::HashSet;
use std::sync::Arc;

use evalit::{Environment, Error, Interpreter, Module, RuntimeError, VM, compile, eval};

fn eval_int(script: &str) -> i64 {
    eval::<i64>(script, Environment::new())
        .unwrap_or_else(|err| panic!("script failed: {script}\n{err:?}"))
        .unwrap_or_else(|| panic!("script returned no value: {script}"))
}

fn run_module(module: Arc<Module>, env: Environment) -> i64 {
    let mut vm = VM::new(module, env);
    #[cfg(not(feature = "async"))]
    let ret = vm.run().unwrap();

    #[cfg(feature = "async")]
    let ret = futures::executor::block_on(vm.run()).unwrap();

    ret.unwrap().take().into_inner::<i64>().ok().unwrap()
}

fn compile_script(script: &str) -> Arc<Module> {
    compile(script, &Environment::new()).unwrap()
}

// ---------------------------------------------------------------------------
// 1. 编译产物结构不变量
// ---------------------------------------------------------------------------

const STRUCTURE_CASES: &[&str] = &[
    "return 1 + 2;",
    r#"
    fn add(a, b) { return a + b; }
    let x = add(1, 2);
    if x > 2 { return x; } else { return 0; }
    "#,
    r#"
    let sum = 0;
    for i in 0..=8 {
        if i % 2 == 0 { sum += i; } else { sum -= i; };
    };
    return sum;
    "#,
    r#"
    fn fact(n) {
        if n <= 1 { return 1; }
        return n * fact(n - 1);
    }
    return fact(6);
    "#,
    r#"
    let arr = [1, 2, 3];
    let m = {"a": 4};
    m.insert("b", 5);
    return arr.len() + m["a"] + m["b"];
    "#,
];

#[test]
fn function_entries_are_inside_the_instruction_stream() {
    for script in STRUCTURE_CASES {
        let module = compile_script(script);
        for &entry in module.symtab.values() {
            assert!(
                entry < module.instructions.len(),
                "function entry {entry} out of bounds for {script}"
            );
        }
    }
}

#[test]
fn branch_targets_stay_inside_the_instruction_stream() {
    for script in STRUCTURE_CASES {
        let module = compile_script(script);
        let len = module.instructions.len() as isize;
        for (pc, inst) in module.instructions.iter().enumerate() {
            let opcode = inst.opcode.to_string();
            let offsets: Vec<usize> = match opcode.as_str() {
                "br" => vec![0],
                "br_if" => vec![1, 2],
                _ => vec![],
            };
            for index in offsets {
                let raw = inst.operands[index].to_string();
                if let Ok(offset) = raw.parse::<isize>() {
                    let target = pc as isize + offset;
                    assert!(
                        (0..len).contains(&target),
                        "branch at pc {pc} jumps to {target} (len {len}) in: {script}"
                    );
                }
            }
        }
    }
}

#[test]
fn instruction_stream_ends_with_a_terminator() {
    for script in STRUCTURE_CASES {
        let module = compile_script(script);
        let last = module.instructions.last().unwrap().to_string();
        assert!(
            last.starts_with("halt") || last.starts_with("ret"),
            "unexpected final instruction `{last}` for {script}"
        );
    }
}

#[test]
fn bytecode_has_no_stale_dedicated_return_register() {
    for script in STRUCTURE_CASES {
        let module = compile_script(script);
        for (pc, inst) in module.instructions.iter().enumerate() {
            let text = inst.to_string();
            assert!(
                !text.contains("rv"),
                "pc {pc} still references the removed Rv register: {text}"
            );
        }
    }
}

#[test]
fn constant_pool_contains_no_duplicates() {
    for script in STRUCTURE_CASES {
        let module = compile_script(script);
        let mut seen = HashSet::new();
        for constant in &module.constants {
            assert!(
                seen.insert(format!("{constant}")),
                "duplicate constant in pool for {script}"
            );
        }
    }

    let module = compile_script(r#"let a = "dup"; let b = "dup"; let c = "other"; return a;"#);
    assert!(
        !module.constants.is_empty(),
        "string literals must enter the pool"
    );
    assert_eq!(module.constants.len(), 2);
}

// ---------------------------------------------------------------------------
// 2. 编译确定性
// ---------------------------------------------------------------------------

#[test]
fn repeated_compilation_is_byte_for_byte_identical() {
    for script in STRUCTURE_CASES {
        let baseline = format!("{}", compile_script(script));
        for _ in 0..20 {
            assert_eq!(
                format!("{}", compile_script(script)),
                baseline,
                "non-deterministic compilation for: {script}"
            );
        }
    }
}

#[test]
fn same_module_produces_the_same_result_on_fresh_vms() {
    for script in [
        "return 1 + 2;",
        "fn f(x) { return x * 3; } return f(14);",
        "let s = 0; for i in 0..=5 { s += i; } return s;",
    ] {
        let module = compile_script(script);
        let expected = run_module(module.clone(), Environment::new());
        for _ in 0..3 {
            assert_eq!(run_module(module.clone(), Environment::new()), expected);
        }
    }
}

// ---------------------------------------------------------------------------
// 3. 与独立参考实现对照
// ---------------------------------------------------------------------------

#[test]
fn loop_sums_match_the_closed_form() {
    for n in 0..=12i64 {
        let script = format!("let s = 0; for i in 0..={n} {{ s += i; }} return s;");
        assert_eq!(eval_int(&script), n * (n + 1) / 2, "n = {n}");
    }
}

#[test]
fn recursive_fibonacci_matches_an_iterative_reference() {
    for n in 0..=10i64 {
        let script = format!(
            "fn fib(k) {{ if k <= 0 {{ return 0; }} if k <= 2 {{ return 1; }} return fib(k-1) + fib(k-2); }} return fib({n});"
        );
        // 标准 fib：f(0)=0, f(1)=1, f(2)=1, f(3)=2 ...
        let mut a = 0i64;
        let mut b = 1i64;
        let expected = match n {
            0 => 0,
            1 => 1,
            _ => {
                for _ in 2..=n {
                    let next = a + b;
                    a = b;
                    b = next;
                }
                b
            }
        };
        assert_eq!(eval_int(&script), expected, "fib({n})");
    }
}

#[test]
fn nested_loop_products_match_a_double_sum() {
    for n in 0..=5i64 {
        let script = format!(
            "let t = 0; for i in 0..={n} {{ for j in 0..={n} {{ t = t + i * j; }} }} return t;"
        );
        let half = n * (n + 1) / 2;
        assert_eq!(eval_int(&script), half * half, "n = {n}");
    }
}

#[test]
fn arithmetic_expressions_match_host_evaluation() {
    let cases: &[(&str, i64)] = &[
        ("return 1 + 2 * 3 - 4;", 3),
        ("return (1 + 2) * (3 - 4);", -3),
        ("return 17 / 5 + 17 % 5;", 5),
        ("return -8 + 3 * -2;", -14),
        ("return 2 * 3 * 4 * 5;", 120),
        ("return 100 - 50 - 25;", 25),
    ];
    for (script, expected) in cases {
        assert_eq!(eval_int(script), *expected, "{script}");
    }
}

#[test]
fn deeply_nested_parentheses_do_not_lose_values() {
    let mut script = String::from("return ");
    script.push_str(&"(".repeat(32));
    script.push_str("1 + 2");
    script.push_str(&")".repeat(32));
    script.push(';');
    assert_eq!(eval_int(&script), 3);

    let mut chained = String::from("let x = 0;\n");
    for _ in 0..64 {
        chained.push_str("x = x + 1;\n");
    }
    chained.push_str("return x;");
    assert_eq!(eval_int(&chained), 64);
}

// ---------------------------------------------------------------------------
// 4. 错误分层与输入边界
// ---------------------------------------------------------------------------

#[test]
fn compile_errors_cover_parse_semantic_and_type_layers() {
    for script in [
        "let = ;",                     // parse
        "return missing_variable;",    // semantic
        "let x = 1 + true; return x;", // type
    ] {
        assert!(
            matches!(
                Interpreter::eval(script, Environment::new()),
                Err(Error::Compile(_))
            ),
            "expected a compile error for: {script}"
        );
    }
}

#[test]
fn runtime_errors_are_typed_not_just_panics() {
    let bad_type = eval::<i64>("return true;", Environment::new()).unwrap_err();
    assert!(matches!(
        bad_type,
        Error::Runtime(RuntimeError::InvalidType { .. })
    ));

    let overflow = Interpreter::eval(
        r#"
        fn recurse(n) {
            if n > 0 { return recurse(n - 1); }
            return 0;
        }
        return recurse(10000);
        "#,
        Environment::new(),
    )
    .unwrap_err();
    assert!(matches!(
        overflow,
        Error::Runtime(RuntimeError::StackOverflow)
    ));
}

#[test]
fn mixed_whitespace_and_comments_do_not_change_results() {
    // 该语言只支持 // 行注释，这里混合 CRLF、制表符、空行与行尾注释。
    let script =
        "\r\n\t// leading comment\r\n  let a = 2;  // trailing\r\n\r\n  return a * 21;\r\n";
    assert_eq!(eval_int(script), 42);

    assert_eq!(eval_int("\n\n  return 7;\r\n"), 7);
    assert_eq!(eval_int("// only a comment\nreturn 3;"), 3);
}

#[test]
fn interpreter_and_typed_eval_agree_on_a_set_of_programs() {
    for script in [
        "return 1 + 2;",
        "let s = 0; for i in 0..=4 { s += i; } return s;",
        "fn f(a) { return a * a; } return f(9);",
    ] {
        let via_interpreter = Interpreter::eval(script, Environment::new())
            .unwrap()
            .unwrap();
        let via_typed = eval::<i64>(script, Environment::new()).unwrap().unwrap();
        assert_eq!(via_interpreter, via_typed, "{script}");
    }
}

// ---------------------------------------------------------------------------
// 5. 审查回归：字面量、短路求值、循环控制流、闭包与函数声明
// ---------------------------------------------------------------------------

#[test]
fn string_and_char_literals_are_unescaped_correctly() {
    let strings: &[(&str, &str)] = &[
        (r#"return "a\"";"#, "a\""),
        (r#"return "a\nb\t\\";"#, "a\nb\t\\"),
        (r#"return "中\u{6587}";"#, "中文"),
        (r#"return "\'";"#, "'"),
    ];
    for (script, expected) in strings {
        let value = eval::<String>(script, Environment::new())
            .unwrap_or_else(|err| panic!("{script}: {err:?}"))
            .unwrap();
        assert_eq!(value, *expected, "{script}");
    }

    // 旧实现取的是字面量开头的单引号，所有字符都成了 '\''。
    let chars: &[(&str, char)] = &[
        ("return 'a';", 'a'),
        (r"return '\n';", '\n'),
        ("return '中';", '中'),
        (r"return '\'';", '\''),
        (r"return '\u{4e2d}';", '中'),
    ];
    for (script, expected) in chars {
        let value = eval::<char>(script, Environment::new())
            .unwrap_or_else(|err| panic!("{script}: {err:?}"))
            .unwrap();
        assert_eq!(value, *expected, "{script}");
    }
}

#[test]
fn logical_operators_short_circuit() {
    let cases: &[(&str, bool)] = &[
        ("return false && true;", false),
        ("return true || false;", true),
        ("return true && 2 > 1;", true),
        ("return false || 2 > 1;", true),
        ("return false && (1 / 0 == 0);", false),
        ("return true || (1 / 0 == 0);", true),
        ("let a = [1]; return false && a[9] == 1;", false),
        ("let a = [1]; return true || a[9] == 1;", true),
    ];
    for (script, expected) in cases {
        let value = eval::<bool>(script, Environment::new())
            .unwrap_or_else(|err| panic!("{script}: {err:?}"))
            .unwrap();
        assert_eq!(value, *expected, "{script}");
    }
}

#[test]
fn continue_in_while_reevaluates_condition_and_pops_loop_context() {
    // 旧实现把 continue 指向循环体开头，条件不再求值，脚本死循环。
    assert_eq!(
        eval_int("let i = 0; while i < 5 { i = i + 1; continue; } return i;"),
        5
    );
    // while 不弹 loop context 时，嵌套在 for 里会污染外层 continue 的目标。
    assert_eq!(
        eval_int(
            "let i = 0; for j in 0..=2 { let k = 0; while k < 1 { k = k + 1; } i = i + 1; continue; } return i;"
        ),
        3
    );
}

#[test]
fn distinct_closures_keep_distinct_bodies() {
    // 匿名闭包签名相同会共用 FunctionId，后一个函数体覆盖前一个。
    assert_eq!(
        eval_int("let f = || { return 1; }; let g = || { return 2; }; return f() + g() * 10;"),
        21
    );
}

#[test]
fn calls_with_more_than_eight_arguments_work() {
    assert_eq!(
        eval_int(
            "fn f(a,b,c,d,e,f,g,h,i) { return a+b+c+d+e+f+g+h+i; } return f(1,2,3,4,5,6,7,8,9);"
        ),
        45
    );
}

#[test]
fn functions_can_be_referenced_before_definition() {
    assert_eq!(eval_int("let g = f; fn f() { return 7; } return g();"), 7);
}

#[test]
fn grammar_constructs_without_runtime_support_do_not_generate_illegal_bytecode() {
    for script in [
        "if true { 1 } else { 2 }",        // block 里的裸表达式
        "struct S {}",                     // 空字段列表
        "if true { struct S { a: int } }", // 块内 item
        "let a: null; return a;",          // type_null
    ] {
        assert!(
            Interpreter::eval(script, Environment::new()).is_ok(),
            "should evaluate without panicking: {script}"
        );
    }
}
