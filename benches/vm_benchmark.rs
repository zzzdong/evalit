use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use evalit::{Environment, Interpreter, Module, Object, VM, compile};

fn run_script(code: &str) -> Result<(), String> {
    Interpreter::eval(code, Environment::new());

    Ok(())
}

fn run_vm<T: Object>(program: Arc<Module>, env: Environment) -> T {
    let mut vm = VM::new(program, env);

    #[cfg(not(feature = "async"))]
    return vm.run().unwrap().unwrap().take().into_inner().unwrap();

    #[cfg(feature = "async")]
    return futures::executor::block_on(async {
        vm.run()
            .await
            .unwrap()
            .unwrap()
            .take()
            .into_inner()
            .unwrap()
    });
}

fn bench_simple_math(c: &mut Criterion) {
    c.bench_function("simple math", |b| {
        b.iter(|| {
            run_script("1 + 2 * 3 - 4 / 5;").unwrap();
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

    let env = Environment::new();
    let module = compile(script, &env).map_err(|e| e.to_string()).unwrap();

    c.bench_function("fibonacci", |b| {
        b.iter(|| {
            let ret = run_vm::<i64>(module.clone(), env.clone());
            assert_eq!(ret, 55);
        })
    });
}

fn bench_native_fibonacci(c: &mut Criterion) {
    fn fib(n: i64) -> i64 {
        if n <= 1 {
            return n;
        }
        return fib(n - 1) + fib(n - 2);
    }

    c.bench_function("native fibonacci", |b| {
        b.iter(|| {
            fib(10);
        })
    });
}

// 注册新的 benchmark 到 criterion_group!
criterion_group!(
    benches,
    bench_simple_math,
    bench_function_call,
    bench_array_index_access,
    bench_fibonacci,
    bench_native_fibonacci,
);
criterion_main!(benches);
