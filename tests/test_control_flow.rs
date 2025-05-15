mod utils;
use evalit::{Environment, Interpreter};
use utils::init_logger;

#[test]
fn test_if_statement() {
    init_logger();

    let env = Environment::new();

    // 测试if-else语句
    let script = r#"
    let a = 10;
    let result = "";
    
    if a == 10 {
        result = "a is 10";
    } else if a == 20 {
        result = "a is 20";
    } else {
        result = "a is neither 10 nor 20";
    };
    
    // 验证if-else结果
    if result != "a is 10" {
        return false;
    };
    
    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_loop_statement() {
    init_logger();

    let env = Environment::new();

    // 测试loop循环
    let script = r#"
    let i = 0;
    loop {
        i = i + 1;
        if i == 5 {
            break;
        };
    };
    
    // 验证循环是否正常退出
    if i != 5 {
        return false;
    };
    
    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_while_statement() {
    init_logger();

    let env = Environment::new();

    // 测试while循环
    let script = r#"
    let j = 0;
    while j < 5 {
        j = j + 1;
    };
    
    // 验证while循环结果
    if j != 5 {
        return false;
    };
    
    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_for_statement() {
    init_logger();

    let env = Environment::new();

    // 测试for循环
    let script = r#"
    let sum = 0;
    for k in 0..=10 {
        if k == 5 {
            continue;
        };
        
        if k == 8 {
            break;
        };
        
        sum = sum + k;
    };
    
    // 验证for循环结果
    // sum = 0+1+2+3+4+6+7 = 23
    if sum != 23 {
        return false;
    };
    
    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_break_continue() {
    init_logger();

    let env = Environment::new();

    // 测试break和continue语句
    let script = r#"
    // 测试简单循环中的break
    let i = 0;
    loop {
        i = i + 1;
        if i < 5 {
            continue;
        };
        
        break;
    };
    
    // 验证循环是否正常退出
    if i != 5 {
        return false;
    };
    
    // 测试简单循环中的continue
    let j = 0;
    loop {
        j = j + 1;
        if j == 3 {
            break;
        };
        
        if j == 2 {
            continue;
        };
    };
    
    // 验证continue是否生效
    if j != 3 {
        return false;
    };
    
    // 测试嵌套循环中的break
    let m = 0;
    let n = 0;
    loop {
        n = 0;
        loop {
            n = n + 1;
            if n < 3 {
                continue;
            };
            
            break;
        };
        
        if n != 3 {
            return false;
        };
        
        m = m + 1;
        if m == 2 {
            break;
        };
    };

    // 验证嵌套循环结果
    if m != 2 || n != 3 {
        return false;
    };
    
    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_return_statement() {
    init_logger();

    let env = Environment::new();

    // 测试函数返回值
    let script = r#"
    fn test_return() {
        return 42;
    };
    
    let ret_val = test_return();
    if ret_val != 42 {
        return false;
    };
    
    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_eval_control_flow() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    let sum = 0;
    for i in 0..=10 {
        if i % 2 == 0 {
            continue;
        }
        sum += i;

        if i == 5 {
            break;
        }
    }
    
    // 验证sum的值是否符合预期（1+3+5）
    if sum != 9 {
        return false;
    }
    
    return true;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    assert_eq!(retval, true);
}

#[test]
fn test_eval_for() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    // 测试数组迭代
    let arr = [1, 2, 3, 4, 5];
    let sum = 0;
    for ele in arr {
        sum += ele;
    }
    
    // 验证数组迭代求和结果
    if sum != 15 {
        return false;
    }
    
    // 测试带索引的数组迭代
    let index_sum = 0;
    let value_sum = 0;
    for (i, ele) in arr.iter().enumerate() {
        index_sum += i;
        value_sum += ele;
    }
    
    // 验证索引和元素迭代的正确性
    if index_sum != 0+1+2+3+4 || value_sum != 15 {
        return false;
    }
    
    // 测试map迭代
    let map = {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5};
    let key_sum = 0;
    let value_sum = 0;
    for (k, v) in map {
        // 这里只是验证迭代可以进行
        key_sum = key_sum + 1; // 计数key的数量
        value_sum += v;
    }
    
    // 验证map迭代结果
    if key_sum != 5 || value_sum != 15 {
        return false;
    }
    
    return true;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    assert_eq!(retval, true);
}
