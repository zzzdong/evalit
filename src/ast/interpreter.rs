use std::{cell::RefMut, collections::HashMap};

use crate::{
    Error, Null, Object, RuntimeError, Value, ValueRef,
    vm::{Enumerator, Environment, Range},
};

use super::{ast::*, parse_file, parser::parse_expression_input};

#[derive(Debug, Clone, Copy)]
struct UserFunction(usize);

impl Object for UserFunction {}

struct Variables {
    scopes: Vec<HashMap<String, ValueRef>>,
}

impl Variables {
    pub fn new() -> Self {
        Variables {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn define(&mut self, name: impl ToString, value: ValueRef) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), value);
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<ValueRef> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name.as_ref()) {
                return Some(value.clone());
            }
        }

        None
    }

    pub fn set(&mut self, name: impl AsRef<str>, value: ValueRef) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(v) = scope.get_mut(name.as_ref()) {
                *v = value;
                return;
            }
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}

#[derive(Debug, Clone)]
enum ControlFlow {
    Next,
    Break,
    Continue,
    Return(Option<ValueRef>),
}

pub struct Interpreter {
    variables: Variables,
    functions: Vec<FunctionItem>,
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: Variables::new(),
            functions: Vec::new(),
            environment: Environment::default(),
        }
    }

    pub fn with_environment(mut self, environment: Environment) -> Self {
        self.environment = environment;
        self
    }

    pub fn get_variable(&self, name: impl AsRef<str>) -> Option<ValueRef> {
        self.variables.get(name)
    }

    pub fn eval(&mut self, script: &str) -> Result<Option<ValueRef>, Error> {
        let program = parse_file(script)?;

        self.eval_program(program)
    }

    pub fn eval_expression_input(&mut self, expr: &str) -> Result<ValueRef, Error> {
        let expr = parse_expression_input(expr)?;

        self.eval_expression(&expr)
    }

    pub fn eval_program(&mut self, program: Program) -> Result<Option<ValueRef>, Error> {
        let mut stmts = Vec::new();

        // split program into statements and items
        for stmt in program.stmts {
            match stmt {
                Statement::Item(ItemStatement::Fn(func)) => {
                    let f = UserFunction(self.functions.len());
                    self.variables
                        .define(func.name.clone(), ValueRef::new(f.into()));
                    self.functions.push(func);
                }
                Statement::Item(_) => {
                    unimplemented!("unsupported item statement")
                }
                Statement::Empty => {}
                _ => {
                    stmts.push(stmt);
                }
            }
        }

        for stmt in stmts {
            match self.eval_statement(&stmt)? {
                ControlFlow::Next => {}
                ControlFlow::Return(val) => {
                    return Ok(val);
                }
                _ => {
                    break;
                }
            }
        }

        Ok(None)
    }

    fn eval_statements(&mut self, stmts: &[Statement]) -> Result<ControlFlow, Error> {
        self.variables.enter_scope();

        for stmt in stmts {
            match self.eval_statement(stmt)? {
                ControlFlow::Next => {}
                control => {
                    self.variables.exit_scope();
                    return Ok(control);
                }
            }
        }

        self.variables.exit_scope();

        Ok(ControlFlow::Next)
    }

    fn eval_statement(&mut self, stmt: &Statement) -> Result<ControlFlow, Error> {
        match stmt {
            Statement::Let(LetStatement { name, ty, value }) => {
                match value {
                    Some(value) => {
                        let value = self.eval_expression(value)?;
                        self.variables.define(name.clone(), value);
                    }
                    None => {
                        self.variables
                            .define(name.clone(), ValueRef::new(Null.into()));
                    }
                }

                Ok(ControlFlow::Next)
            }
            Statement::Return(ReturnStatement { value }) => match value {
                Some(value) => {
                    let value = self.eval_expression(value)?;
                    Ok(ControlFlow::Return(Some(value)))
                }
                None => Ok(ControlFlow::Return(None)),
            },
            Statement::If(IfStatement {
                condition,
                then_branch,
                else_branch,
            }) => {
                let condition = self.eval_expression(condition)?;
                if condition == true {
                    return self.eval_statements(then_branch);
                } else if let Some(else_branch) = else_branch {
                    return self.eval_statements(else_branch);
                }

                Ok(ControlFlow::Next)
            }
            Statement::Loop(LoopStatement { body }) => {
                loop {
                    match self.eval_statements(body)? {
                        ControlFlow::Break => {
                            break;
                        }
                        ControlFlow::Continue => {
                            continue;
                        }
                        ControlFlow::Next => {}
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                    }
                }

                Ok(ControlFlow::Next)
            }

            Statement::While(WhileStatement { condition, body }) => {
                loop {
                    let condition = self.eval_expression(condition)?;
                    if condition == false {
                        break;
                    }

                    match self.eval_statements(body)? {
                        ControlFlow::Break => {
                            break;
                        }
                        ControlFlow::Continue => {
                            continue;
                        }
                        ControlFlow::Next => {}
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                    }
                }

                Ok(ControlFlow::Next)
            }

            Statement::For(ForStatement {
                pat,
                iterable,
                body,
            }) => {
                let iterable = self.eval_expression(iterable)?;
                let mut iterable = Enumerator::new(iterable.get().make_iterator()?);

                loop {
                    if !(iterable.iterator_has_next()?) {
                        break;
                    }

                    let value = iterable.iterate_next()?;
                    match pat {
                        Pattern::Identifier(ident) => {
                            self.variables.define(ident.clone(), value);
                        }
                        Pattern::Wildcard => {}
                        _ => unimplemented!("unsupported pattern: {:?}", pat),
                    };

                    match self.eval_statements(body)? {
                        ControlFlow::Break => {
                            break;
                        }
                        ControlFlow::Continue => {
                            continue;
                        }
                        ControlFlow::Next => {}
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                    }
                }

                Ok(ControlFlow::Next)
            }
            Statement::Break => Ok(ControlFlow::Break),
            Statement::Continue => Ok(ControlFlow::Continue),
            Statement::Expression(expr) => {
                self.eval_expression(expr)?;
                Ok(ControlFlow::Next)
            }
            _ => unimplemented!("unsupported statement: {:?}", stmt),
        }
    }

    fn eval_expression(&mut self, expr: &Expression) -> Result<ValueRef, Error> {
        match expr {
            Expression::Literal(lit) => Ok(self.eval_literal(lit)),
            Expression::Identifier(ident) => self
                .variables
                .get(ident.0.as_str())
                .ok_or_else(|| RuntimeError::symbol_not_found(ident.0.as_str()).into()),
            Expression::Environment(env) => self
                .environment
                .get(env.0.as_str())
                .ok_or_else(|| RuntimeError::symbol_not_found(env.0.as_str()).into()),
            Expression::Prefix(expr) => self.eval_prefix_expression(expr),
            Expression::Binary(expr) => self.eval_binary_expression(expr),
            Expression::Assign(expr) => self.eval_assign_expression(expr),
            Expression::Call(expr) => self.eval_call_expression(expr),
            _ => unimplemented!("unsupported expression: {:?}", expr),
        }
    }

    fn eval_call_expression(&mut self, expr: &CallExpression) -> Result<ValueRef, Error> {
        let CallExpression { func, args } = expr;

        match func.as_ref() {
            Expression::Identifier(ident) => {
                if let Some(mut func) = self.get_variable(ident.0.as_str()) {
                    let mut func = func.get_mut();
                    match func.must_downcast_mut::<UserFunction>() {
                        Ok(func) => self.eval_function_call(func, args),
                        Err(_) => self.eval_call_callable(func, args),
                    }
                } else {
                    Err(RuntimeError::symbol_not_found(ident.0.as_str()).into())
                }
            }
            Expression::Environment(env) => match self.environment.get(env.0.as_str()) {
                Some(mut func) => {
                    let func = func.get_mut();
                    self.eval_call_callable(func, args)
                }
                None => Err(RuntimeError::symbol_not_found(env.0.as_str()).into()),
            },
            _ => unimplemented!("unsupported call expression: {:?}", func),
        }
    }

    fn eval_function_call(
        &mut self,
        func: &UserFunction,
        args: &[Expression],
    ) -> Result<ValueRef, Error> {
        let UserFunction(id) = func;
        match self.functions.get(*id).cloned() {
            Some(FunctionItem {
                name,
                params,
                return_ty,
                body,
            }) => {
                self.variables.enter_scope();
                let args = args
                    .iter()
                    .map(|arg| self.eval_expression(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                for (param, arg) in params.iter().zip(args.iter()) {
                    self.variables.define(param.name.as_str(), arg.clone());
                }

                if let ControlFlow::Return(ret) = self.eval_statements(&body)? {
                    self.variables.exit_scope();
                    return Ok(ret.unwrap_or(ValueRef::new(Null.into())));
                }
                self.variables.exit_scope();
                Ok(ValueRef::new(Null.into()))
            }
            None => Err(RuntimeError::symbol_not_found(func.0).into()),
        }
    }

    fn eval_call_callable(
        &mut self,
        mut callable: RefMut<Value>,
        args: &[Expression],
    ) -> Result<ValueRef, Error> {
        let args = args
            .iter()
            .map(|arg| self.eval_expression(arg))
            .collect::<Result<Vec<_>, _>>()?;
        let ret = callable.call(&args).map(|ret| ret.unwrap_or(Null.into()))?;
        Ok(ValueRef::new(ret))
    }

    fn eval_literal(&self, lit: &LiteralExpression) -> ValueRef {
        match lit {
            LiteralExpression::Null => ValueRef::new(Value::new(Null)),
            LiteralExpression::Boolean(b) => ValueRef::new(Value::new(*b)),
            LiteralExpression::Char(c) => ValueRef::new(Value::new(*c)),
            LiteralExpression::Integer(i) => ValueRef::new(Value::new(*i)),
            LiteralExpression::Float(f) => ValueRef::new(Value::new(*f)),
            LiteralExpression::String(s) => ValueRef::new(Value::new(s.clone())),
        }
    }

    fn eval_prefix_expression(&mut self, expr: &PrefixExpression) -> Result<ValueRef, Error> {
        let PrefixExpression { op, rhs } = expr;

        let rhs = self.eval_expression(rhs)?;

        match op {
            PrefixOp::Not => {
                let value = rhs.value().negate()?;
                Ok(ValueRef::new(value))
            }
            PrefixOp::Neg => {
                let value = rhs.value().negate()?;
                Ok(ValueRef::new(value))
            }
        }
    }

    fn eval_binary_expression(&mut self, expr: &BinaryExpression) -> Result<ValueRef, Error> {
        let BinaryExpression { op, lhs, rhs } = expr;

        let left = self.eval_expression(lhs)?;
        let right = self.eval_expression(rhs)?;
        self.eval_binop(op, &left, &right)
    }

    fn eval_binop(&self, op: &BinOp, left: &ValueRef, right: &ValueRef) -> Result<ValueRef, Error> {
        let value = match op {
            BinOp::Add => left.get().add(&right.get())?,
            BinOp::Sub => left.get().sub(&right.get())?,
            BinOp::Mul => left.get().mul(&right.get())?,
            BinOp::Div => left.get().div(&right.get())?,
            BinOp::Mod => left.get().rem(&right.get())?,
            BinOp::LogicAnd => left.get().logic_and(&right.get())?,
            BinOp::LogicOr => left.get().logic_or(&right.get())?,
            BinOp::Equal => Value::new(left.get().compare(&right.get())?.is_eq()),
            BinOp::NotEqual => Value::new(left.get().compare(&right.get())?.is_ne()),
            BinOp::Greater => Value::new(left.get().compare(&right.get())?.is_gt()),
            BinOp::GreaterEqual => Value::new(left.get().compare(&right.get())?.is_ge()),
            BinOp::Less => Value::new(left.get().compare(&right.get())?.is_lt()),
            BinOp::LessEqual => Value::new(left.get().compare(&right.get())?.is_le()),
            BinOp::Range => {
                let range = Range::new(left.clone(), right.clone(), false)?;
                Value::new(range)
            }
            BinOp::RangeInclusive => {
                let range = Range::new(left.clone(), right.clone(), true)?;
                Value::new(range)
            }
            _ => unimplemented!("unsupported binary operator: {:?}", op),
        };
        Ok(value.into())
    }

    fn eval_assign_expression(&mut self, expr: &AssignExpression) -> Result<ValueRef, Error> {
        let AssignExpression { object, value } = expr;

        let value = self.eval_expression(value)?;
        if let Expression::Identifier(ident) = object.as_ref() {
            self.variables.set(ident.0.as_str(), value);
            Ok(ValueRef::new(Null.into()))
        } else {
            // TODO: implement more assign expressions, e.g. object.field = value, object[index] = value, $env = value, etc.
            unimplemented!("unsupported assign expression: {:?}", object);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::assert_value_eq;

    #[test]
    fn test_null_literal() {
        let mut interp = Interpreter::new();
        let expr = r#"null"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, Null);
    }

    #[test]
    fn test_bool_literal() {
        let mut interp = Interpreter::new();
        let expr = r#"true"#;
        let value = interp.eval_expression_input(expr).unwrap();

        assert_value_eq(value, true);

        let expr = r#"false"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, false);
    }

    #[test]
    fn test_number_literal() {
        let mut interp = Interpreter::new();

        let expr = r#"123"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, 123);

        let expr = r#"-123"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, -123);

        let expr = r#"1.23"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, 1.23);

        let expr = r#"-1.23"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, -1.23);
    }

    #[test]
    fn test_eval_string_literal() {
        let mut interp = Interpreter::new();

        let expr = r#""Hello, World!""#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, "Hello, World!");

        let expr = r#""Hello, \nWorld!""#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, "Hello, \nWorld!");
    }

    #[test]
    fn test_eval_prefix_expression() {
        let mut interp = Interpreter::new();

        let expr = "!true";
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, false);

        let expr = "!false";
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, true);

        let expr = "-1";
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, -1);
    }

    #[test]
    fn test_eval_binary_expression() {
        let mut interp = Interpreter::new();

        // 测试加法运算符
        let expr = r#"1 + 2"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, 3);

        // 测试减法运算符
        let expr = r#"5 - 3"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, 2);

        // 测试乘法运算符
        let expr = r#"4 * 3"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, 12);

        // 测试除法运算符
        let expr = r#"10 / 2"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, 5);

        // 测试取模运算符
        let expr = r#"10 % 3"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, 1);

        // 测试逻辑与运算符
        let expr = r#"true && false"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, false);

        // 测试逻辑或运算符
        let expr = r#"true || false"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, true);

        // 测试等于运算符
        let expr = r#"1 == 1"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, true);

        // 测试不等于运算符
        let expr = r#"1 != 2"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, true);

        // 测试大于运算符
        let expr = r#"3 > 2"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, true);

        // 测试大于等于运算符
        let expr = r#"3 >= 3"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, true);

        // 测试小于运算符
        let expr = r#"2 < 3"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, true);

        // 测试小于等于运算符
        let expr = r#"2 <= 2"#;
        let value = interp.eval_expression_input(expr).unwrap();
        assert_value_eq(value, true);
    }

    #[test]
    fn test_eval_if_statement() {
        let mut interp = Interpreter::new();

        // 测试简单的 if 语句
        let script = r#"
            let a = 1;
            if a == 1 {
                a = 2;
            }
            return a;
        "#;
        let ret = interp.eval(script).unwrap();
        assert_value_eq(ret.unwrap(), 2);

        // 测试 if-else 语句
        let script = r#"
            let a = 1;
            if a == 2 {
                a = 2;
            } else {
                a = 3;
            }
            return a;
        "#;
        let ret = interp.eval(script).unwrap();
        assert_value_eq(ret.unwrap(), 3);
    }

    #[test]
    fn test_eval_loop_stmt() {
        let mut interp = Interpreter::new();

        // 测试简单的 loop 循环
        let script = r#"
            let a = 0;
            loop {
                a = a + 1;
                if a == 3 {
                    break;
                }
            }
            return a;
        "#;
        let ret = interp.eval(script).unwrap().unwrap();
        assert_value_eq(ret, 3);
    }

    #[test]
    fn test_eval_while_stmt() {
        let mut interp = Interpreter::new();

        // 测试简单的 while 循环
        let script = r#"
            let a = 0;
            while a < 3 {
                a = a + 1;
            }
            return a;
        "#;
        let ret = interp.eval(script).unwrap().unwrap();
        assert_value_eq(ret, 3);
    }

    #[test]
    fn test_eval_for_stmt() {
        let mut interp = Interpreter::new();

        // 测试简单的 for 循环
        let script = r#"
            let a = 0;
            for i in 0..3 {
                a = a + 1;
            }
            return a;
        "#;
        let ret = interp.eval(script).unwrap().unwrap();
        assert_value_eq(ret, 3);
    }

    #[test]
    fn test_eval_for_stmt_with_break() {
        let mut interp = Interpreter::new();

        // 测试 for 循环中带有 break 语句
        let script = r#"
            let a = 0;
            for i in 0..5 {
                if i == 3 {
                    break;
                }
                a = a + 1;
            }
            return a;
        "#;
        let ret = interp.eval(script).unwrap().unwrap();
        assert_value_eq(ret, 3);
    }

    #[test]
    fn test_eval_for_stmt_with_continue() {
        let mut interp = Interpreter::new();

        // 测试 for 循环中带有 continue 语句
        let script = r#"
            let a = 0;
            for i in 0..5 {
                if i % 2 == 0 {
                    continue;
                }
                a = a + 1;
            }
            return a;
        "#;
        let ret = interp.eval(script).unwrap().unwrap();
        assert_value_eq(ret, 2);
    }

    #[test]
    fn test_eval_fn() {
        let mut interp = Interpreter::new();

        let script = r#"
        fn add(a, b) {
            return a + b;
        }

        return add(1, 2);
        "#;
        let ret = interp.eval(script).unwrap().unwrap();
        assert_value_eq(ret, 3);
    }

    #[test]
    fn test_eval_fn_recursive() {
        let mut interp = Interpreter::new();

        // 测试递归函数
        let script = r#"
        fn fib(n) {
            if n < 1 {
                return 0;
            }
            if n <= 2 {
                return 1;
            }

            return fib(n - 1) + fib(n - 2);
        }

        return fib(2);
        "#;
        let ret = interp.eval(script).unwrap().unwrap();
        assert_value_eq(ret, 1);
    }

    #[test]
    fn test_eval_fn_var() {
        let mut interp = Interpreter::new();

        let script = r#"
        fn add(a, b) {
            return a + b;
        }

        let a = add;

        return a(1, 2);
        "#;
        let ret = interp.eval(script).unwrap().unwrap();
        assert_value_eq(ret, 3);
    }

    #[test]
    fn test_eval_env_variable() {
        let env = Environment::new().with_variable("a", 100 as i64);

        let mut interp = Interpreter::new().with_environment(env);

        let script = r#"
        let b = 2;
        return ${a} + b;
        "#;
        let ret = interp.eval(script).unwrap().unwrap();
        assert_value_eq(ret, 102);
    }

    fn fib(n: i64) -> i64 {
        if n < 1 {
            return 0;
        }
        if n <= 2 {
            return 1;
        }

        return fib(n - 1) + fib(n - 2);
    }

    #[test]
    fn test_eval_native_function() {
        let env = Environment::new().with_function("fib", fib);
        let mut interp = Interpreter::new().with_environment(env);

        let script = r#"let a = $fib(5);"#;

        interp.eval(script).unwrap();
        let var_value = interp.get_variable("a").unwrap();
        assert_value_eq(var_value, 5);
    }

    #[test]
    fn test_eval_return_stmt() {
        let mut interp = Interpreter::new();

        let script = r#"return 1;"#;

        let ret = interp.eval(script).unwrap().unwrap();

        assert_value_eq(ret, 1);
    }
}
