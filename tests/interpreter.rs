use evalit::{Environment, Interpreter, RuntimeError};

mod utils;

use utils::init_logger;

#[test]
fn test_eval_undefine() {
    let env = Environment::new();

    let script = r#"
        let x;
        return x;
        "#;

    let retval = Interpreter::eval_script(script, env);
    assert!(retval.is_err());
}

#[test]
fn test_eval_boolean() {
    init_logger();

    let env = Environment::new();

    let script = r#"
        let a = true;
        let b = false;
        return a && !b;
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, true);
}

#[test]
fn test_eval_integer() {
    init_logger();
    let env = Environment::new();

    let script = r#"
        let max = 9223372036854775807;  // 2^63
        let min = -9223372036854775807; // -2^63
        return max + min;
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, 0);
}

#[test]
fn test_eval_float() {
    let env = Environment::new();

    let script = r#"
        let pi = 3.141592653589793;
        let e = 2.718281828459045;
        return pi * e;
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    let diff = (*retval.downcast_ref::<f64>().unwrap() - 8.539734222673567).abs();
    println!("diff = {diff}");
    assert!(diff < 1e-10);
}

#[test]
fn test_eval_string() {
    let env = Environment::new();

    let script = r#"
        let greeting = "Hello, ";
        let name = "World!";
        return greeting + name;
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, "Hello, World!");
}

#[test]
fn test_eval_loop() {
    let env = Environment::new();

    let script = r#"
        let i = 1;
        let sum = 0;
        loop {
            sum += i;
            i += 1;
            if i > 10 {
                break;
            }
        }
        return sum;
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, 55);
}

#[test]
fn test_eval_for_loop() {
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
fn test_eval_if_statement() {
    let env = Environment::new();

    let script = r#"
        let x = 10;
        if x > 5 {
            return "Greater than 5";
        } else {
            return "Less than or equal to 5";
        }
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, "Greater than 5");
}

#[test]
fn test_eval_break_continue() {
    let env = Environment::new();

    let script = r#"
        let sum = 0;
        for i in 0..10 {
            if i == 5 {
                break;
            }
            if i % 2 == 0 {
                continue;
            }
            sum += i;
        }
        return sum;
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, 4); // 1 + 3
}

#[test]
fn test_eval_for_with_if() {
    let env = Environment::new();

    let script = r#"
        let sum = 0;
        for i in 0..10 {
            if i % 2 == 0 {
                sum += i;
            }
        }
        return sum;
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, 20); // 0 + 2 + 4 + 6 + 8
}

#[test]
fn test_eval_if_multiple_conditions() {
    let env = Environment::new();

    let script = r#"
        let x = 10;
        let y = 5;
        if x > 5 && y < 10 || x == 10 {
            return "Condition met";
        } else {
            return "Condition not met";
        }
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, "Condition met");
}

#[test]
fn test_eval_map_iteration() {
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
fn test_eval_null_variable() {
    let env = Environment::new();

    let script = r#"
        return x;
        "#;

    let result = Interpreter::eval_script(script, env);
    assert!(result.is_err());
}

#[test]
fn test_eval_invalid_index_access() {
    let env = Environment::new();

    let script = r#"
        let map = {"a": 1, "b": 2};
        return map["c"];
        "#;

    let result = Interpreter::eval_script(script, env);
    assert!(result.is_err());
}

#[test]
fn test_eval_invalid_type_conversion() {
    let env = Environment::new();

    let script = r#"
        let str = "123";
        return str + 1;
        "#;

    let result = Interpreter::eval_script(script, env);
    assert!(result.is_err());
}

#[test]
fn test_eval_simple_function_call() {
    let env = Environment::new();

    let script = r#"
        fn add(a, b) {
            return a + b;
        }

        return add(3, 5);
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, 8);
}

#[test]
fn test_eval_function_with_multiple_parameters() {
    let env = Environment::new();

    let script = r#"
        fn multiply(a, b, c) {
            return a * b * c;
        }

        return multiply(2, 3, 4);
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, 24);
}

#[test]
fn test_eval_recursive_function_fibonacci() {
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

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, 143);
}

#[test]
fn test_eval_recursive_function_factorial() {
    let env = Environment::new();

    let script = r#"
        fn factorial(n) {
            if n <= 1 {
                return 1;
            }
            return n * factorial(n - 1);
        }

        return factorial(5);
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, 120);
}

#[test]
fn test_eval_higher_order_function() {
    init_logger();
    let env = Environment::new();

    let script = r#"
        fn apply_twice(f, x) {
            return f(f(x));
        }

        fn increment(x) {
            return x + 1;
        }

        return apply_twice(increment, 5);
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, 7); // (5 + 1) + 1 = 7
}

#[test]
fn test_eval_null_function_call() {
    let env = Environment::new();

    let script = r#"
        return unknown_function();
        "#;

    let result = Interpreter::eval_script(script, env);
    assert!(result.is_err());
}

#[test]
fn test_eval_recursive_depth_exceeded() {
    let env = Environment::new();

    let script = r#"
        fn recursive(n) {
            if n > 0 {
                return recursive(n - 1);
            }
            return 0;
        }

        return recursive(10000);  // 递归深度过大
        "#;

    let result = Interpreter::eval_script(script, env);
    assert!(matches!(result.err().unwrap(), evalit::Error::Runtime(RuntimeError::StackOverflow)));
}

#[test]
fn test_eval_function_call_chain() {
    let env = Environment::new();

    let script = r#"
        fn add(a, b) {
            return a + b;
        }

        fn multiply(a, b) {
            return a * b;
        }

        return multiply(add(2, 3), add(4, 5));
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, 45); // (2 + 3) * (4 + 5) = 5 * 9 = 45
}

#[test]
fn test_eval_function_with_control_flow() {
    let env = Environment::new();

    let script = r#"
        fn calculate(n) {
            let result = 0;
            for i in 1..=n {
                if i % 2 == 0 {
                    result += i;
                } else {
                    result -= i;
                }
            }
            return result;
        }

        return calculate(5);
        "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();
    assert_eq!(retval, -3); // (-1) + 2 + (-3) + 4 + (-5) = -3
}
