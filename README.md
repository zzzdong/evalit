evalit
======

A toy interpreter.

# example

```rust
use evalit::{Interpreter, Evaluator};

fn main() {
    let mut env = Environment::new();

    let script = r#"
    fn fib(n) {
        if n <= 0 {
            return 0;
        }
        if n <= 2 {
            return 1;
        }

        return fib(n-1) + fib(n-2);
    }

    let sum = 0;
    for i in 0..=10 {
        sum += fib(i);
    }

    return sum;
    "#;

    let retval = Interpreter::eval_script(script, env).unwrap();

    println!("ret: {:?}", ret); 
    // should output: 
    // ret: Some(143)
}


```

# syntax

## primitive type

### null

### boolean

`true`, `false`

### integer

$-2^{63}$ ~ $2^{63}$

### float

float number, f64.

### string

string quote by `"`


## variable

variable name is a string, start with `_` or letter, and can contain letter, number, `_`.

```rust
let a;
let a = 1;
```

## expression

```rust
1 + 2 * 3;
```

### operator


| operator | description |
| -------- | ----------- |
| `+` | add |
| `-` | subtract |
| `*` | multiply |
| `/` | divide |
| `%` | remainder |
| `==` | equal |
| `!=` | not equal |
| `<` | less than |
| `<=` | less than or equal |
| `>` | greater than |
| `>=` | greater than or equal |
| `&&` | and |
| `\|\|` | or |
| `!` | not |
| `=` | assign |
| `+=` | add assign |
| `-=` | subtract assign |
| `*=` | multiply assign |
| `/=` | divide assign |
| `%=` | remainder assign |
| `[]` | index |
| `.` | member |
| `()` | call |
| `..` | range |


## control flow

### loop statement

`loop` to repeat.

```rust
loop {}
```

### while statement

`while` to conditional repeat.

```rust
while condition {}
```

### for statement

`for` to iterate.

```rust
for i in 0..=10 {}
```

### if statement

`if` to choose branch.

```rust
if condition {} else {}
```

### break statement

`break` to exist loop.

### continue statement

`continue` to finish one iterate.

### return statement

`return` to return value.

## fn item

### user defined function

```rust
fn fib(n) {
    if n <= 0 {
        return 0;
    }
    if n <= 2 {
        return 1;
    }

    return fib(n-1) + fib(n-2);
}
```
