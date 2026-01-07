mod utils;
use evalit::{Environment, Interpreter, Null, Value};
use utils::init_logger;

#[test]
fn test_null_basic() {
    init_logger();

    // 1. 直接返回null
    let env = Environment::new();
    let script = r#"return null;"#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, Null);

    // 2. null赋值给未声明类型的变量
    let env = Environment::new();
    let script = r#"
    let x = null;
    return x;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, Null);

    // 3. null赋值给Any类型变量
    let env = Environment::new();
    let script = r#"
    let x: any = null;
    return x;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, Null);

    // 4. null不能赋值给具体类型变量 - 应该编译错误
    let env = Environment::new();
    let script = r#"
    let x: int = null;
    return x;
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_err());
    println!("Expected error for null assignment to int: {:?}", ret);
}

#[test]
fn test_null_comparison() {
    init_logger();

    // 1. null == null 应该为true
    let env = Environment::new();
    let script = r#"
    return null == null;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, true);

    // 2. null != null 应该为false
    let env = Environment::new();
    let script = r#"
    return null != null;
    "#;
    let retval = Interpreter::eval(script, env).unwrap().unwrap();
    assert_eq!(retval, false);
}

#[test]
fn test_null_operations() {
    init_logger();

    // 1. null不能参与算术运算
    let test_cases = vec![
        "null + 1", "1 + null", "null - 1", "null * 2", "null / 2", "null % 2",
    ];

    for op in test_cases {
        let env = Environment::new();
        let script = format!("return {};", op);
        let ret = Interpreter::eval(&script, env);
        assert!(ret.is_err(), "Should reject {} operation", op);
        println!("Expected error for {}: {:?}", op, ret);
    }

    // 2. null不能参与逻辑运算
    let logic_tests = vec!["null && true", "true && null", "null || false", "!null"];

    for op in logic_tests {
        let env = Environment::new();
        let script = format!("return {};", op);
        let ret = Interpreter::eval(&script, env);
        assert!(ret.is_err(), "Should reject logical {} operation", op);
        println!("Expected error for {}: {:?}", op, ret);
    }

    // 3. null不能参与关系比较
    let relation_tests = vec!["null < 1", "null <= 1", "null > 1", "null >= 1"];

    for op in relation_tests {
        let env = Environment::new();
        let script = format!("return {};", op);
        let ret = Interpreter::eval(&script, env);
        assert!(ret.is_err(), "Should reject relation {} operation", op);
        println!("Expected error for {}: {:?}", op, ret);
    }
}

#[test]
fn test_type_strictness() {
    init_logger();

    // 1. 禁止隐式数值类型转换
    let conversion_tests = vec![
        ("let x: int = 10; let y: float = x;", "int to float"),
        ("let x: float = 3.14; let y: int = x;", "float to int"),
        ("let x: byte = 1; let y: int = x;", "byte to int"),
        ("let x: int = 10; let y: byte = x;", "int to byte"),
    ];

    for (script, description) in conversion_tests {
        let env = Environment::new();
        let ret = Interpreter::eval(script, env);
        assert!(ret.is_err(), "Should reject {} conversion", description);
        println!("Expected error for {}: {:?}", description, ret);
    }

    // 2. 相同数值类型之间的运算应该允许
    let valid_ops = vec![
        ("return 1 + 2;", "int + int"),
        ("return 3.14 + 2.71;", "float + float"),
        (
            "let x: int = 10; let y: int = 20; return x + y;",
            "int variable addition",
        ),
        (
            "let x: float = 1.5; let y: float = 2.5; return x + y;",
            "float variable addition",
        ),
    ];

    for (script, description) in valid_ops {
        let env = Environment::new();
        let ret = Interpreter::eval(script, env);
        assert!(ret.is_ok(), "Should allow {}", description);
        println!("Valid operation {}: {:?}", description, ret);
    }

    // 3. 不同类型数值之间运算应该拒绝
    let invalid_ops = vec![
        ("return 1 + 3.14;", "int + float"),
        ("return 3.14 - 1;", "float - int"),
        ("return 2 * 3.14;", "int * float"),
        ("return 10.0 / 2;", "float / int"),
    ];

    for (script, description) in invalid_ops {
        let env = Environment::new();
        let ret = Interpreter::eval(script, env);
        assert!(ret.is_err(), "Should reject {}", description);
        println!("Expected error for {}: {:?}", description, ret);
    }

    // 4. 字符串拼接只允许字符串类型
    let env = Environment::new();
    let script = r#"
    let a: string = "hello";
    let b: string = "world";
    return a + b;
    "#;
    let ret = Interpreter::eval(script, env);
    println!("ret: {:?}", ret);
    assert!(ret.is_ok(), "Should allow string concatenation");

    // 5. 字符串和其他类型拼接应该拒绝
    let env = Environment::new();
    let script = r#"return "number: " + 42;"#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_err(), "Should reject string + int");
}

#[test]
fn test_function_type_checking() {
    init_logger();

    // 1. 函数参数类型检查
    let env = Environment::new();
    let script = r#"
    fn add(a: int, b: int) -> int {
        return a + b;
    }
    
    return add(1, 2);
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_ok(), "Should allow correct function call");

    // 2. 函数参数类型不匹配应该拒绝
    let env = Environment::new();
    let script = r#"
    fn add(a: int, b: int) -> int {
        return a + b;
    }
    
    return add(1, 2.5);
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_err(), "Should reject wrong argument type");

    // 3. 函数参数传递null应该拒绝（除非参数类型是any）
    let env = Environment::new();
    let script = r#"
    fn process(value: int) -> int {
        return value * 2;
    }
    
    return process(null);
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(
        ret.is_err(),
        "Should reject null argument for int parameter"
    );

    // 4. 函数参数类型为any时允许null
    let env = Environment::new();
    let script = r#"
    fn process(value: any) -> any {
        return value;
    }
    
    return process(null);
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_ok(), "Should allow null argument for any parameter");
    assert_eq!(ret.unwrap().unwrap(), Null);
}

#[test]
fn test_variable_declaration() {
    init_logger();

    // 1. 未声明类型的变量默认值为null，类型为Any
    let env = Environment::new();
    let script = r#"
    let x;
    // x应该是Any类型，值为null
    return x == null;
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_ok());
    assert_eq!(ret.unwrap().unwrap(), true);

    // 2. 声明类型但未赋值的变量
    let env = Environment::new();
    let script = r#"
    let x: int;
    // x应该是int类型，但值可能未初始化（取决于实现）
    return x;  // 这里可能有问题，取决于未初始化变量的行为
    "#;
    let ret = Interpreter::eval(script, env);
    // 这个测试取决于未初始化变量的具体实现

    // 3. 类型推断
    let env = Environment::new();
    let script = r#"
    let x = 42;        // 应该推断为int
    let y = 3.14;      // 应该推断为float
    let z = "hello";   // 应该推断为string
    let n = null;      // 应该推断为any
    
    return x + 10;     // 应该允许，因为x是int
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_ok());
    // 应该返回52
}

#[test]
fn test_struct_and_enum_types() {
    init_logger();

    // 1. 结构体字段类型检查
    let env = Environment::new();
    let script = r#"
    struct Person {
        name: string,
        age: int,
    }
    
    let p = Person { name: "Alice", age: 30 };
    return p.age;
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_ok());

    // 2. 结构体字段类型不匹配应该拒绝
    let env = Environment::new();
    let script = r#"
    struct Person {
        name: string,
        age: int,
    }
    
    let p = Person { name: "Bob", age: "thirty" };
    return p;
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_err(), "Should reject wrong field type");

    // 3. 结构体字段赋值为null应该拒绝
    let env = Environment::new();
    let script = r#"
    struct Person {
        name: string,
        age: int,
    }
    
    let p = Person { name: null, age: 25 };
    return p;
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_err(), "Should reject null for string field");
}

#[test]
fn test_array_type_checking() {
    init_logger();

    // 1. 数组元素类型应该一致
    let env = Environment::new();
    let script = r#"
    let arr = [1, 2, 3];
    return arr[0] + arr[1];
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_ok());

    // 2. 混合类型的数组应该被检测（如果实现类型检查）
    let env = Environment::new();
    let script = r#"
    let arr = [1, "two", 3.0];
    return arr;
    "#;
    let ret = Interpreter::eval(script, env);
    // 这个测试取决于数组的类型检查严格程度
    // 如果严格检查，应该出错；如果宽松，可能允许

    // 3. 包含null的数组
    let env = Environment::new();
    let script = r#"
    let arr = [null, null, null];
    return arr[0] == null;
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_ok());
    assert_eq!(ret.unwrap().unwrap(), true);
}

#[test]
fn test_edge_cases() {
    init_logger();

    // 1. 变量重新赋值类型检查
    let env = Environment::new();
    let script = r#"
    let x = 10;        // x: int
    x = 20;           // 允许，同类型
    x = 3.14;         // 应该拒绝，类型不匹配
    return x;
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_err());

    // 2. 循环中的类型检查
    let env = Environment::new();
    let script = r#"
    let sum = 0;
    for i in 0..10 {
        sum = sum + i;  // sum和i都应该是int
    }
    return sum;
    "#;
    let ret = Interpreter::eval(script, env);
    println!("ret: {:?}", ret);
    assert!(ret.is_ok());

    // 3. 返回类型检查
    let env = Environment::new();
    let script = r#"
    fn getNumber() -> int {
        return "not a number";  // 应该拒绝
    }
    return getNumber();
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_err());
}

#[test]
fn test_any_type_behavior() {
    init_logger();

    // 1. Any类型可以接受任何值
    let env = Environment::new();
    let script = r#"
    let x: any = 42;
    let y: any = 3.14;
    let z: any = "hello";
    let n: any = null;
    
    // 可以重新赋值为不同类型的值
    x = "now a string";
    y = 100;
    
    return x;
    "#;
    let ret = Interpreter::eval(script, env);

    assert!(ret.is_ok());

    // 2. Any类型不能赋给具体类型
    let env = Environment::new();
    let script = r#"
    let x: any = 42;
    let y: int = x;  // 应该拒绝
    return y;
    "#;
    let ret = Interpreter::eval(script, env);
    assert!(ret.is_err());

    // 3. Any类型参与运算
    let env = Environment::new();
    let script = r#"
    let x: any = 10;
    let y: any = 20;
    return x + y;  //
    "#;
    let ret = Interpreter::eval(script, env);
    // 这个测试取决于Any类型参与运算的具体规则
    assert!(ret.is_ok());

    // 3. Any类型参与运算
    let env = Environment::new();
    let script = r#"
    let x: any = 10;
    let y: any = "hello";
    return x + y;  //
    "#;
    let ret = Interpreter::eval(script, env);
    // 这个测试取决于Any类型参与运算的具体规则
    assert!(ret.is_err());
}
