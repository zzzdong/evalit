mod utils;
use evalit::{Environment, Interpreter, Null};
use utils::init_logger;

// #[test]
// fn test_struct_type() {
//     init_logger();

//     let script = r#"
//     struct Person {
//         name: string,
//         age: int,
//     }

//     let person = Person {
//         name: "Alice",
//         age: 18,
//     };

//     return person.name == "Alice" && person.age == 18;
//     "#;

//     let result = Interpreter::eval(script, Environment::new()).unwrap().unwrap();

//     assert_eq!(result, true);
// }