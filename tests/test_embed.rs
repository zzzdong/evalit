mod utils;
use evalit::{compile, Environment, Error, Module, Null, Program, Value, ValueRef, VM};
use utils::init_logger;

fn run_vm(program: &Module, env: &Environment) -> Result<Value, Error> {
    let mut vm = VM::new(program, env);
    #[cfg(feature = "async")]
    let ret = futures::executor::block_on(async { vm.run().await })?;
    #[cfg(not(feature = "async"))]
    let ret = vm.run()?;

    Ok(ret.unwrap().take())
}

#[test]
fn test_basic_embedding() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    // 定义一个简单的函数
    fn add(a, b) {
        return a + b;
    }
    
    // 执行一些计算
    let result1 = add(2, 3);
    let result2 = add(result1, 5);
    
    // 返回最终结果
    return result2;
    "#;

    let program = compile(script, &env).unwrap();

    let retval = run_vm(&program, &env).unwrap();

    assert_eq!(retval, 10);
}

#[test]
fn test_rust_interop() {
    init_logger();

    // 定义一个Rust函数
    fn greet(name: String) -> String {
        format!("Hello, {}!", name)
    }

    let mut env = Environment::new();

    // 将Rust函数注册到脚本环境中
    env.define_function("greet", greet);

    let script = r#"
    // 使用Rust函数
    let message = greet("World");
    
    // 操作返回值
    if message != "Hello, World!" {
        return false;
    }
    
    // 测试组合使用
    let composed = greet("Script") + " Welcome to Evalit";
    if composed != "Hello, Script! Welcome to Evalit" {
        return false;
    }
    
    return true;
    "#;

    let program = compile(script, &env).unwrap();
    let retval = run_vm(&program, & env).unwrap();

    assert_eq!(retval, true);
}

#[test]
fn test_multiple_vm_instances() {
    init_logger();

    let env = Environment::new();

    let script = r#"
    fn add(a, b) {
        return a + b;
    }
    
    return add(1, 1);
    "#;

    let program = compile(script, &env).unwrap();

    let retval1 = run_vm(&program, &env).unwrap();
    let retval2 = run_vm(&program, &env).unwrap();

    assert_eq!(retval1, 2);
    assert_eq!(retval2, 2);
}

#[test]
fn test_shared_compiled_program() {
    init_logger();

    let env = Arc::new(Environment::new());
    
    // 创建一个可共享的程序
    let script = r#"
    fn add(a, b) {
        return a + b;
    }
    
    // 测试基础调用
    return add(2, 3);
    "#;

    // 编译一次
    let program = compile(script, &env).unwrap();
    
    // 在多个线程中复用编译后的程序
    use std::sync::Arc;
    use std::thread;
    
    // 包装成Arc以安全共享
    let shared_program = Arc::new(program);
    let mut handles = vec![];
    
    for _ in 0..5 {
        let program_clone = Arc::clone(&shared_program);
        let env_clone = env.clone();
        
        let handle = thread::spawn(move || {
            // 每个线程创建一个新的VM来运行相同的程序
            let result = run_vm(&program_clone, &env_clone);
            result.unwrap()
        });
        
        handles.push(handle);
    }
    
    // 验证所有线程都返回正确结果
    for handle in handles {
        let retval = handle.join().unwrap();
        assert_eq!(retval, 5);
    }
}
