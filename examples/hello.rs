use evalit::{Environment, Interpreter, ValueRef};

fn main() {
    let mut env = Environment::new();

    env.define_function("println", println);

    let script = r#"
    println("hello, world");

    let sum = 0;
    for i in 0..=10 {
        sum += i;
    }

    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap();

    println!("ret: {retval:?}");
}

fn println(args: &[ValueRef]) {
    let s = args
        .iter()
        .map(|v| format!("{v}"))
        .collect::<Vec<String>>()
        .join("");

    println!("{s}");
}
