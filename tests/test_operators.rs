mod utils;
use evalit::{Environment, Interpreter};
use utils::init_logger;

#[test]
fn test_arithmetic_operators() {
    init_logger();

    let env = Environment::new();

    // 测试算术运算符
    let script = r#"
    if 2 + 3 != 5 {
        return false;
    };
    
    if 5 - 2 != 3 {
        return false;
    };
    
    if 3 * 4 != 12 {
        return false;
    };
    
    if 10 / 2 != 5 {
        return false;
    };
    
    if 10 % 3 != 1 {
        return false;
    };
    
    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_unary_operators() {
    init_logger();

    let env = Environment::new();

    // 测试一元运算符
    let script = r#"
    let a = 5;
    
    if -a != -5 {
        return false;
    };
    
    if !(true) != false {
        return false;
    };
    
    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_comparison_operators() {
    init_logger();

    let env = Environment::new();

    // 测试比较运算符
    let script = r#"
    if 1 == 1 == false {
        return false;
    };
    
    if 1 != 2 == false {
        return false;
    };
    
    if 3 < 4 == false {
        return false;
    };
    
    if 4 > 3 == false {
        return false;
    };
    
    if 4 <= 4 == false {
        return false;
    };
    
    if 5 >= 5 == false {
        return false;
    };
    
    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_logical_operators() {
    init_logger();

    let env = Environment::new();

    // 测试逻辑运算符
    let script = r#"
    // 测试逻辑与
    if true && true != true {
        return false;
    };
    
    if true && false != false {
        return false;
    };
    
    if false && true != false {
        return false;
    };
    
    if false && false != false {
        return false;
    };
    
    // 测试逻辑或
    if (true || true) != true {
        return false;
    };
    
    if (true || false) != true {
        return false;
    };
    
    if (false || true) != true {
        return false;
    };
    
    if (false || false) != false {
        return false;
    };

    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_assignment_operators() {
    init_logger();

    let env = Environment::new();

    // 测试赋值运算符
    let script = r#"
    let b = 0;
    
    b += 5;
    if b != 5 {
        return false;
    };
    
    b -= 3;
    if b != 2 {
        return false;
    };
    
    b *= 4;
    if b != 8 {
        return false;
    };
    
    b /= 2;
    if b != 4 {
        return false;
    };
    
    b %= 3;
    if b != 1 {
        return false;
    };
    
    return true;
    "#;
    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, true);
}

#[test]
fn test_range_operations() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    // 测试不同形式的range
    
    // 简单range求和
    let sum = 0;
    for i in 0..5 {
        sum += i;
    }
    if sum != 10 {
        return false;
    }
    
    // 测试包含上界的range
    let sum = 0;
    for i in 0..=5 {
        sum += i;
    }
    if sum != 15 {
        return false;
    }
    
    // 测试带变量的range
    let start = 2;
    let end = 5;
    let sum = 0;
    for i in start..end {
        sum += i;
    }
    if sum != 12 {
        return false;
    }
    
    // 测试空range
    let count = 0;
    for i in 5..3 {
        count += 1;
    }
    if count != 0 {
        return false;
    }
    
    // 测试长度为0的range
    let range = 0..0;
    if range.len() != 0 {
        return false;
    }
    
    // 测试长度为非零的range
    let range = 0..5;
    if range.len() != 5 {
        return false;
    }
    
    return true;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    assert_eq!(retval, true);
}

#[test]
fn test_slice_operations() {
    init_logger();

    let env = Environment::new();


    let script = r#"
    let array = [1, 2, 3, 4, 5];
    // 验证不同的切片形式并验证结果
    
    // 全部元素
    let slice1 = array[..];
    if slice1.len() != 5 || slice1[0] != 1 || slice1[1] != 2 || slice1[2] != 3 || slice1[3] != 4 || slice1[4] != 5 {
        return false;
    }
    
    // 从索引1到末尾
    let slice2 = array[1..];
    if slice2.len() != 4 || slice2[0] != 2 || slice2[1] != 3 || slice2[2] != 4 || slice2[3] != 5 {
        return false;
    }
    
    // 从开头到索引3（不包含3）
    let slice3 = array[..3];
    if slice3.len() != 3 || slice3[0] != 1 || slice3[1] != 2 || slice3[2] != 3 {
        return false;
    }
    
    // 从索引1到3（不包含3）
    let slice4 = array[1..3];
    if slice4.len() != 2 || slice4[0] != 2 || slice4[1] != 3 {
        return false;
    }
    
    // 从索引1到3（包含3）
    let slice5 = array[1..=3];
    if slice5.len() != 3 || slice5[0] != 2 || slice5[1] != 3 || slice5[2] != 4 {
        return false;
    }
    
    // 验证切片求和
    let sum = 0;
    for i in slice5 {
        sum += i;
    }
    
    // 返回验证结果
    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    assert_eq!(retval, 9);
}

#[test]
fn test_make_iterator() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    // 测试数组迭代器
    let arr = [1, 2, 3, 4, 5];
    let iter = arr.iter();
    
    // 验证next方法
    if iter.next() != 1 || iter.next() != 2 || iter.next() != 3 || iter.next() != 4 || iter.next() != 5 || iter.next() != null {
        return false;
    }
    
    // 测试再次迭代
    let iter = arr.iter();
    
    // 验证next方法
    if iter.next() != 1 || iter.next() != 2 || iter.next() != 3 || iter.next() != 4 || iter.next() != 5 || iter.next() != null {
        return false;
    }


    return true;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap().unwrap();

    assert_eq!(retval, true);
}
