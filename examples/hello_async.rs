use evalit::{Environment, Interpreter, Value, ValueRef};

#[cfg(feature = "async")]
use evalit::Promise;

#[cfg(feature = "async")]
#[tokio::main]
async fn main() {
    let mut env = Environment::new();

    env.define_function("println", println);
    env.define_function("http_get", http_get);

    let script = r#"
    println("hello, world");

    let sum = 0;
    for i in 0..=10 {
        sum += i;
    }

    let resp = http_get("https://bing.com").await;


    println(resp);


    return sum;
    "#;

    let retval = Interpreter::eval_script_async(script, env).await.unwrap();

    println!("ret: {retval:?}");
}

#[cfg(feature = "async")]
fn http_get(url: String) -> Promise {
    use evalit::Promise;

    Promise::new(async move {
        println!("url: {url:?}");

        let resp = reqwest::get(url).await.unwrap();
        let body = resp.text().await.unwrap();

        Value::new(body)
    })
}

fn println(args: &[ValueRef]) {
    let s = args
        .iter()
        .map(|v| format!("{v}"))
        .collect::<Vec<String>>()
        .join("");

    println!("{s}");
}
