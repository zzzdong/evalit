mod utils;
use evalit::{Environment, Interpreter};
use utils::init_logger;

#[test]
fn test_struct_type_simple() {
    init_logger();

    let script = r#"
    struct Person {
        name: string,
        age: int,
    }

    let person = Person {
        name: "Alice",
        age: 18,
    };

    return person.name == "Alice" && person.age == 18;
    "#;

    let result = Interpreter::eval(script, Environment::new())
        .unwrap()
        .unwrap();

    assert_eq!(result, true);
}

#[test]
fn test_struct_decl() {
    init_logger();

    let script = r#"
    struct Point {
        x: int,
        y: int,
    }

    struct Rectangle {
        point: Point,
        width: int,
        height: int,
        un_init: any,
    }

    let a = Rectangle {
        point: Point {
            x: 1,
            y: 2,
        },
        width: 3,
        height: 4,
    };


    return a.point.x == 1 && a.point.y == 2 && a.width == 3 && a.height == 4 && a.un_init == null;
    "#;

    let result = Interpreter::eval(script, Environment::new())
        .unwrap()
        .unwrap();

    assert_eq!(result, true);
}

#[test]
fn test_nested_struct() {
    init_logger();

    let script = r#"
    struct Address {
        street: string,
        city: string,
        zip: int,
    }

    struct Person {
        name: string,
        age: int,
        address: Address,
    }

    let person = Person {
        name: "Bob",
        age: 25,
        address: Address {
            street: "123 Main St",
            city: "Anytown",
            zip: 12345,
        },
    };

    return person.name == "Bob" && person.age == 25 && person.address.street == "123 Main St" && person.address.city == "Anytown" && person.address.zip == 12345;
    "#;

    let result = Interpreter::eval(script, Environment::new())
        .unwrap()
        .unwrap();

    assert_eq!(result, true);
}

#[test]
fn test_struct_array() {
    init_logger();

    let script = r#"
    struct Point {
        x: int,
        y: int,
    }

    let points = [
        Point { x: 1, y: 2 },
        Point { x: 3, y: 4 },
        Point { x: 5, y: 6 },
    ];

    return points[0].x == 1 && points[0].y == 2 && points[1].x == 3 && points[1].y == 4 && points[2].x == 5 && points[2].y == 6;
    "#;

    let result = Interpreter::eval(script, Environment::new())
        .unwrap()
        .unwrap();

    assert_eq!(result, true);
}

#[test]
fn test_struct_mutual_reference() {
    init_logger();

    let script = r#"
    struct Person {
        name: string,
        address: Address,
    }

    struct Address {
        street: string,
        city: string,
        owner: Person,
    }

    let person = Person {
        name: "Alice",
        address: Address {
            street: "123 Main St",
            city: "Anytown",
            owner: Person {
                name: "Bob",
                address: Address {
                    street: "456 Elm St",
                    city: "Othertown",
                    owner: Person {
                        name: "Charlie",
                        address: null,
                    },
                },
            },
        },
    };

    return person.name == "Alice" && person.address.street == "123 Main St" && person.address.city == "Anytown" && person.address.owner.name == "Bob" && person.address.owner.address.street == "456 Elm St" && person.address.owner.address.city == "Othertown" && person.address.owner.address.owner.name == "Charlie";
    "#;

    let result = Interpreter::eval(script, Environment::new())
        .unwrap()
        .unwrap();

    assert_eq!(result, true);
}
