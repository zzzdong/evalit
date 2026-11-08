//! 字节码 golden 快照：SSA / 寄存器分配 / 指令选择的验收基线。
//!
//! 快照按「不同进程」比对，能抓到 `HashMap`/`HashSet` 迭代顺序造成的
//! 非确定性编译（同一进程内编译两次用的是同一个哈希种子，抓不到）。
//!
//! 更新快照：`EVALIT_UPDATE_GOLDEN=1 cargo test --test test_bytecode_golden`
use std::{env, fs, path::PathBuf};

use evalit::{Compiler, Environment, Module};

const CASES: &[(&str, &str)] = &[
    (
        "arith",
        r#"
        let a = 1 + 2 * 3;
        let b = a - 4;
        return b;
        "#,
    ),
    (
        "if_phi",
        r#"
        let x = 3;
        let y = 0;
        if x > 0 {
            y = x + 1;
        } else {
            y = x - 1;
        }
        return y;
        "#,
    ),
    (
        "loop_carried",
        r#"
        let sum = 0;
        for i in 0..=10 {
            sum = sum + i;
        }
        return sum;
        "#,
    ),
    (
        "nested_loops",
        r#"
        let total = 0;
        for i in 0..=3 {
            for j in 0..=3 {
                total = total + i * j;
            }
        }
        return total;
        "#,
    ),
    (
        "branch_in_loop",
        r#"
        let acc = 0;
        for i in 0..=5 {
            if i % 2 == 0 {
                acc = acc + i;
            } else {
                acc = acc - i;
            };
        };
        return acc;
        "#,
    ),
    (
        "function",
        r#"
        fn add(a, b) {
            return a + b;
        }
        let r = add(2, 3);
        return r;
        "#,
    ),
    ("many_live_values", MANY_LIVE_VALUES),
];

/// 高寄存器压力：变量数超过通用寄存器数，覆盖溢出 / 驱逐 / 栈槽交接。
/// 修复前 `must_alloc()` 无空闲寄存器会直接 panic。
const MANY_LIVE_VALUES: &str = r#"
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

/// 只 dump 与执行语义相关的部分；调试用 IR 克隆不进快照。
fn dump(module: &Module) -> String {
    let mut out = String::new();

    out.push_str("=== constants ===\n");
    for (i, constant) in module.constants.iter().enumerate() {
        out.push_str(&format!("{i}\t: {constant}\n"));
    }

    out.push_str("=== symtab ===\n");
    let mut symtab: Vec<_> = module.symtab.iter().collect();
    symtab.sort_by_key(|(_, index)| **index);
    for (id, index) in symtab {
        out.push_str(&format!("{id}: {index}\n"));
    }

    out.push_str("=== instructions ===\n");
    for (i, inst) in module.instructions.iter().enumerate() {
        out.push_str(&format!("{i}\t: {inst}\n"));
    }

    out
}

#[test]
fn bytecode_golden_is_stable() {
    let update = env::var_os("EVALIT_UPDATE_GOLDEN").is_some();
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/golden/bytecode");

    for &(name, script) in CASES {
        let module = Compiler::new()
            .compile(script, &Environment::new())
            .unwrap_or_else(|err| panic!("compile `{name}` failed: {err:?}"));
        let actual = dump(&module);
        let path = dir.join(format!("{name}.txt"));

        if update {
            fs::create_dir_all(&dir).unwrap();
            fs::write(&path, &actual).unwrap();
            continue;
        }

        let expected = fs::read_to_string(&path).unwrap_or_else(|err| {
            panic!(
                "missing golden `{}` ({err}); run with EVALIT_UPDATE_GOLDEN=1",
                path.display()
            )
        });
        // checkout 时的换行符转换不应造成假失败。
        let expected = expected.replace("\r\n", "\n");
        assert_eq!(actual, expected, "bytecode changed for `{name}`");
    }
}
