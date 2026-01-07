mod utils;
use evalit::{Environment, Interpreter, Null};
use utils::init_logger;

#[test]
fn test_null() {
    init_logger();

    let env = Environment::new();
    let script = r#"
    return null;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, Null);

    let env = Environment::new();
    let script = r#"
    let x = null;
    return x;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, Null);

    let env = Environment::new();
    let script = r#"
    let x: int = null;
    return x;
    "#;
    let ret = Interpreter::eval(script, env);

    println!("ret: {:?}", ret);
    assert!(ret.is_err());
}

#[test]
fn test_boolean() {
    init_logger();

    let env = Environment::new();

    // 测试true
    let script = r#"
    return true;
    "#;

    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, true);

    let env = Environment::new();

    // 测试false
    let script = r#"
    return false;
    "#;

    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, false);
}

#[test]
fn test_integer() {
    init_logger();

    let env = Environment::new();

    {
        // 测试正整数
        let script = r#"
        return 42;
        "#;
        let retval = Interpreter::eval(script, env).unwrap().unwrap();
        assert_eq!(retval, 42);
    }
    {
        let env = Environment::new();
        // 测试负整数
        let script = r#"
    return -17;
    "#;
        let retval = Interpreter::eval(script, env).unwrap().unwrap();
        assert_eq!(retval, -17);
    }
}

#[test]
fn test_float() {
    init_logger();

    let env = Environment::new();

    // 测试浮点数
    let script = r#"
    return 3.1415;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, 3.1415);
}

#[test]
fn test_string() {
    init_logger();

    let env = Environment::new();

    // 测试字符串
    let script = r#"
    return "hello world";
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();

    println!("ret: {:?}", retval);
    assert_eq!(retval, "hello world");
}

#[test]
fn test_array_methods() {
    init_logger();

    let env = Environment::new();

    // 测试数组初始化和基本操作
    let script = r#"
    let arr = [1, 2, 3];
    
    // 测试数组长度
    if arr.len() != 3 {
        return false;
    };
    
    // 测试push方法
    arr.push(4);
    if arr.len() != 4 || arr[3] != 4 {
        return false;
    };
    
    // 测试pop方法
    let val = arr.pop();
    if val != 4 || arr.len() != 3 {
        return false;
    };
    
    // 测试remove方法
    let val = arr.remove(1);
    if val != 2 || arr.len() != 2 || arr[0] != 1 || arr[1] != 3 {
        return false;
    };
    
    // 测试数组迭代求和
    let sum = 0;
    for i in arr {
        sum += i;
    }
    
    if sum != 4 {
        return false;
    };
    
    // 测试enumerate迭代
    let arr = [1, 2, 3, 4, 5];
    let sum = 0;
    for (i, ele) in arr.iter().enumerate() {
        sum += i;
        sum += ele;
    }
    
    if sum != 0+1+1+2+2+3+3+4+4+5 {
        return false;
    };
    
    return true;
    "#;

    let retval = Interpreter::eval(script, env).unwrap().unwrap();

    assert_eq!(retval, true);
}

#[test]
fn test_map_basics() {
    init_logger();

    let env = Environment::new();

    // 测试map创建和访问
    let script = r#"
    let person = {"name": "Alice", "age": 30};
    
    // 验证基本功能
    if person["name"] != "Alice" || person["age"] != 30 {
        return false;
    };
    
    // 测试添加新属性
    person["score"] = 100;
    if person.len() != 3 || person["score"] != 100 {
        return false;
    };
    
    // 测试修改现有属性
    person["age"] = 31;
    if person["age"] != 31 {
        return false;
    };
    
    return true;
    "#;

    let retval = Interpreter::eval(script, env).unwrap().unwrap();

    assert_eq!(retval, true);
}

#[test]
fn test_map_methods() {
    init_logger();

    let env = Environment::new();

    // 测试map的方法
    let script = r#"
    let person = {};
    
    // 测试insert方法
    person.insert("name", "Alice");
    person.insert("age", 30);
    person.insert("city", "New York");
    
    if person.len() != 3 {
        return false;
    };
    
    if person["name"] != "Alice" || person["age"] != 30 || person["city"] != "New York" {
        return false;
    };
    
    // 测试remove方法
    person.remove("city");
    
    if person.len() != 2 {
        return false;
    }
    
    // 测试keys方法
    let keys = person.keys();
    if keys.len() != 2 {
        return false;
    };
    
    // 验证keys的正确性
    let has_name = false;
    let has_age = false;
    for key in keys {
        if key == "name" {
            has_name = true;
        } else if key == "age" {
            has_age = true;
        }
    }
    
    if !has_name || !has_age {
        return false;
    }
    
    // 测试values方法
    let values = person.values();
    if values.len() != 2 {
        return false;
    }
    
    // 验证values的正确性
    let has_Alice = false;
    let has_30 = false;
    for value in values {
        if value == "Alice" {
            has_Alice = true;
        } else if value == 30 {
            has_30 = true;
        }
    }
    
    return has_Alice && has_30;
    "#;

    let retval = Interpreter::eval(script, env).unwrap().unwrap();

    assert_eq!(retval, true);
}
