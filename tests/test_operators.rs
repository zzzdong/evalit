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