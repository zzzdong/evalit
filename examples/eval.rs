use evalit::{Environment, ValueRef, eval_blocking};

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

    let retval = eval_blocking::<i64>(script, env).unwrap();

    println!("ret: {retval:?}");

    assert_eq!(retval, Some(55));
}

fn println(args: &[ValueRef]) {
    let s = args
        .iter()
        .map(|v| format!("{v}"))
        .collect::<Vec<String>>()
        .join("");

    println!("{s}");
}
