use criterion::{Criterion, criterion_group, criterion_main};
use evalit::{Environment, VM, compile};

fn run_script(code: &str) -> Result<(), String> {
    let env = Environment::new();
    let module = compile(code, &env).map_err(|e| e.to_string())?;
    let mut vm = VM::new(module, env);
    let _ret = futures::executor::block_on(async { vm.run().await }).unwrap();

    Ok(())
}

fn bench_simple_math(c: &mut Criterion) {
    c.bench_function("simple math", |b| {
        b.iter(|| {
            run_script("1 + 2 * 3 - 4 / 5;").unwrap();
        })
    });
}

fn bench_fibonacci(c: &mut Criterion) {
    let script = r#"
    fn fib(n) {
        if n <= 1 {
            return n;
        }
        return fib(n - 1) + fib(n - 2);
    }
    return fib(10);
    "#;

    c.bench_function("fibonacci", |b| {
        b.iter(|| {
            run_script(script).unwrap();
        })
    });
}

fn bench_function_call(c: &mut Criterion) {
    let script = r#"
    fn inc(x) { x + 1; }
    for i in 0..1000 {
        inc(i);
    }
    "#;

    c.bench_function("function call", |b| {
        b.iter(|| {
            run_script(script).unwrap();
        })
    });
}

fn bench_array_index_access(c: &mut Criterion) {
    let script = r#"
    let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    for i in 0..1000 {
        arr[i % 10] = arr[(i + 1) % 10];
    }
    "#;

    c.bench_function("array index access", |b| {
        b.iter(|| {
            run_script(script).unwrap();
        })
    });
}

// 注册新的 benchmark 到 criterion_group!
criterion_group!(
    benches,
    bench_simple_math,
    bench_fibonacci,
    bench_function_call,
    bench_array_index_access,
);
criterion_main!(benches);
