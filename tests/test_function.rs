mod utils;
use evalit::{Environment, Interpreter, RuntimeError};
use utils::init_logger;

#[test]
fn test_basic_function() {
    init_logger();

    let env = Environment::new();

    // 测试无参数无返回值的函数
    let script = r#"
    fn hello() {
        let msg = "Hello";
    }
    hello();
    "#;
    assert!(Interpreter::eval(script, env).is_ok());
}

#[test]
fn test_function_with_params() {
    let env = Environment::new();

    // 测试带参数的函数
    let script = r#"
    fn add(a, b) {
        return a + b;
    }
    
    let result = add(2, 3);
    if result != 5 {
        return false;
    }

    return true;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, true);
}

#[test]
fn test_function_scope() {
    let env = Environment::new();

    // 测试函数作用域
    let script = r#"
    let x = 1;
    fn modify() {
        let x = 2;
        return x;
    }
    let y = modify();
    return x;  // 应该返回外部作用域的x值
    "#;

    let ret = Interpreter::eval(script, env).unwrap();
    assert_eq!(ret.unwrap(), 1);
}

#[test]
fn test_recursive_function() {
    let env = Environment::new();

    // 测试递归函数
    let script = r#"
    fn factorial(n) {
        if n <= 1 {
            return 1;
        }
        return n * factorial(n - 1);
    }
    let result = factorial(5);
    return result;
    "#;
    let ret = Interpreter::eval(script, env).unwrap();
    assert_eq!(ret.unwrap(), 120);
}

#[test]
fn test_fibonacci() {
    let env = Environment::new();

    // 测试README中的fibonacci示例
    let script = r#"
    fn fib(n) {
        if n <= 0 {
            return 0;
        }
        if n <= 2 {
            return 1;
        }
        return fib(n-1) + fib(n-2);
    }
    let sum = 0;
    for i in 0..=10 {
        sum += fib(i);
    }
    return sum;
    "#;
    let ret = Interpreter::eval(script, env).unwrap();
    assert_eq!(ret.unwrap(), 143);
}

#[test]
fn test_multiple_returns() {
    let env = Environment::new();

    // 测试多个return语句
    let script = r#"
    fn max(a, b) {
        if a > b {
            return a;
        }
        return b;
    }
    let result = max(10, 5);
    return result;
    "#;
    let ret = Interpreter::eval(script, env).unwrap();
    assert_eq!(ret.unwrap(), 10);
}

#[test]
fn test_function_simple_call() {
    init_logger();
    let env = Environment::new();

    let script = r#"
        fn add(a, b) {
            return a + b;
        }

        return add(3, 5);
        "#;

    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 8);
}

#[test]
fn test_function_multiple_parameters() {
    init_logger();
    let env = Environment::new();

    let script = r#"
        fn multiply(a, b, c) {
            return a * b * c;
        }

        return multiply(2, 3, 4);
        "#;

    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 24);
}

#[test]
fn test_function_higher_order() {
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

    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 7); // (5 + 1) + 1 = 7
}

#[test]
fn test_function_call_chain() {
    init_logger();
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

    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, 45); // (2 + 3) * (4 + 5) = 5 * 9 = 45
}

#[test]
fn test_function_with_control_flow() {
    init_logger();
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

    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, -3); // (-1) + 2 + (-3) + 4 + (-5) = -3
}

#[test]
fn test_function_recursive_depth_exceeded() {
    init_logger();
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

    let result = Interpreter::eval(script, env);
    assert!(matches!(
        result.err().unwrap(),
        evalit::Error::Runtime(RuntimeError::StackOverflow)
    ));
}

#[test]
fn test_function_mutual_recursion() {
    init_logger();
    let env = Environment::new();

    let script = r#"
        fn is_even(n) {
            if n == 0 {
                return true;
            }
            return is_odd(n - 1);
        }

        fn is_odd(n) {
            if n == 0 {
                return false;
            }
            return is_even(n - 1);
        }

        return is_even(100);  // 应该返回true
        "#;

    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, true);
}

#[test]
fn test_function_tail_call_optimization() {
    init_logger();
    let env = Environment::new();

    let script = r#"
        fn tail_recursive(n, acc) {
            if n == 0 {
                return acc;
            }
            return tail_recursive(n - 1, n * acc);
        }

        return tail_recursive(1000, 1);  // 应该能正常计算而不溢出
        "#;

    let result = Interpreter::eval(script, env);
    // 计算1000!的值太大，我们只验证是否能正常执行而不会栈溢出
    // 这里可以添加更复杂的验证逻辑，但为了简单起见，我们只检查返回值类型

    assert!(matches!(
        result.err().unwrap(),
        evalit::Error::Runtime(RuntimeError::Overflow)
    ));
}
