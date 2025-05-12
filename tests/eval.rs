use evalit::{Environment, Interpreter, ValueRef};

mod utils;

use utils::init_logger;

fn fib(n: i64) -> i64 {
    if n < 1 {
        return 0;
    }
    if n <= 2 {
        return 1;
    }

    fib(n - 1) + fib(n - 2)
}

fn println(args: &[ValueRef]) {
    let s = args
        .iter()
        .map(|v| format!("{v}"))
        .collect::<Vec<String>>()
        .join("");

    println!("{s}");
}

#[test]
fn test_simple() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    let sum = 0;
    sum = 1 + 2 + 3 + 4 + 5;
    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {retval:?}");

    assert_eq!(retval, 15);
}

#[test]
fn test_eval_for_range() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    let sum = 0;
    for i in 0..=10 {
        sum += i;
    }
    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    assert_eq!(retval, 55);
}

#[test]
fn test_eval_control_flow() {
    init_logger();

    let mut env = Environment::new();

    env.define_function("println", println);

    let script = r#"
    let sum = 0;
    for i in 0..=10 {
        println("i=", i);
        if i % 2 == 0 {
            continue;
        }
        sum += i;

        if i == 5 {
            break;
        }
        println("sum=", sum);
    }
    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    assert_eq!(retval, 9);
}

#[test]
fn test_eval_array() {
    init_logger();

    let mut env = Environment::new();

    env.define_function("println", println);

    let script = r#"
    let sum = 0;
    let array = [1, 2, 3, 4, 5];

    println("array.len = ", array.len());

    for (i, ele) in array.enumerate() {
        println("array[", i, "]=", ele);
    }

    for i in array {
        sum += i;
    }

    println("sum = ", sum);
    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    assert_eq!(retval, 15);
}

#[test]
fn test_eval_map() {
    let env = Environment::new();

    let script = r#"
    let sum = 0;
    let map = {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5};
    for (k, v) in map {
        sum += v;
    }
    sum += map["a"];
    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    assert_eq!(retval, 16);
}

#[test]
fn test_slice() {
    init_logger();

    let mut env = Environment::new();

    env.define_function("println", println);

    let script = r#"
    let sum = 0;
    let array = [1, 2, 3, 4, 5];
    // println("array.len = ", array.len());


    let slice = array[..];
    slice = array[1..];
    slice = array[..3];
    slice = array[1..3];
    slice = array[1..=3];

    for i in slice {
        sum += i;
    }

    println("sum = ", sum);


    let str = "hello中文测试";

    for c in str {
        println("->", c);
    }

    // println("str.len = ", str.len());

    println(str[1..=5]);

    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    assert_eq!(retval, 9);
}

#[test]
fn test_eval_for() {
    init_logger();

    let mut env = Environment::new();

    env.define_function("println", println);

    let script = r#"

    let arr = [1, 2, 3, 4, 5];
    for ele in arr {
        println("->", ele);
    }

    for (i, ele) in arr.enumerate() {
        println("[", i, "]->", i);
    }

    let map = {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5};
    for (k, v) in map {
        println(k, "->", v);
    }
    "#;

    let retval = Interpreter::eval_script(script, env);

    println!("ret: {retval:?}");
    assert!(retval.is_ok());
}

#[test]
fn test_eval_env() {
    init_logger();

    let mut env = Environment::new();

    env.define_function("fib", fib);

    let script = r#"
    let sum = 0;
    for i in 1..=10 {
        sum += fib(i);
    }
    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap();

    println!("ret: {retval:?}");
}

#[test]
fn test_eval() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    fn fib(n) {
        if n < 1 {
            return 0;
        }
        if n <= 2 {
            return 1;
        }

        return fib(n - 1) + fib(n - 2);
    }

    let f = fib;

    let sum = 0;
    for i in 1..=10 {
        sum += f(i);
    }

    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap();

    println!("ret: {retval:?}");

    assert_eq!(retval.unwrap(), 143);
}
