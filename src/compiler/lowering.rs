use std::collections::HashMap;

use super::ast::syntax::*;
use super::ir::{builder::*, instruction::*};
use super::typing::{TypeContext, TypeDef};
use crate::compiler::symbol::SymbolTable;
use crate::compiler::typing::{FunctionDef, StructDef};
use crate::{
    Environment,
    bytecode::{FunctionId, Opcode, Primitive},
    runtime::EnvVariable,
};

struct LoopContext {
    pub(crate) break_point: BlockId,
    pub(crate) continue_point: BlockId,
}

impl LoopContext {
    pub(crate) fn new(break_point: BlockId, continue_point: BlockId) -> Self {
        Self {
            break_point,
            continue_point,
        }
    }
}

pub struct ASTLower<'a> {
    builder: &'a mut dyn InstBuilder,
    env: &'a Environment,
    symbols: SymbolTable<Variable>,
    loop_contexts: Vec<LoopContext>,
    type_cx: &'a TypeContext,
}

impl<'a> ASTLower<'a> {
    pub fn new(
        builder: &'a mut dyn InstBuilder,
        symbols: SymbolTable<Variable>,
        env: &'a Environment,
        type_cx: &'a TypeContext,
    ) -> Self {
        Self {
            builder,
            env,
            symbols,
            loop_contexts: Vec::new(),
            type_cx,
        }
    }

    pub fn lower_program(&mut self, prog: Program) -> IrUnit {
        let mut unit = IrUnit::new();

        let builder: &mut dyn InstBuilder = &mut IrBuilder::new(&mut unit);

        let entry = builder.create_block("__entry".into());
        builder.switch_to_block(entry);

        // declare functions
        for func in self.type_cx.functions() {
            self.declare_function(func);
        }

        let entry = self.create_block("main");
        self.builder.switch_to_block(entry);
        self.builder.set_entry(entry);

        // split program into statements and items
        for stmt in prog.stmts {
            match stmt.node {
                Statement::Item(ItemStatement::Function(func)) => {
                    self.lower_function_item(func);
                }
                Statement::Item(_) => {
                    // unimplemented!("unsupported item statement")
                }
                Statement::Empty => {}
                _ => {
                    self.lower_statement(stmt);
                }
            }
        }

        // FIXME: This is a hack to make block not empty.
        self.builder.make_halt();

        unit
    }

    fn lower_statement(&mut self, statement: StatementNode) {
        match statement.node {
            Statement::Expression(expression) => {
                self.lower_expression(expression);
            }
            Statement::Let(let_stmt) => {
                self.lower_let_stmt(let_stmt);
            }
            Statement::Item(stmt) => {
                self.lower_item_stmt(stmt);
            }
            Statement::Return(ReturnStatement { value }) => {
                let value = value.map(|expr| self.lower_expression(expr));
                self.builder.return_(value);
            }
            Statement::If(if_stmt) => {
                self.lower_if_stmt(if_stmt);
            }
            Statement::Loop(loop_stmt) => {
                self.lower_loop_stmt(loop_stmt);
            }
            Statement::While(while_stmt) => {
                self.lower_while_stmt(while_stmt);
            }
            Statement::For(for_stmt) => {
                self.lower_for_stmt(for_stmt);
            }
            Statement::Break => {
                self.lower_break_stmt();
            }
            Statement::Continue => {
                self.lower_continue_stmt();
            }
            Statement::Block(block_stmt) => {
                self.lower_block(block_stmt);
            }
            Statement::Empty => {} // _ => unimplemented!("{:?}", statement),
        }
    }

    fn lower_let_stmt(&mut self, let_stmt: LetStatement) {
        let LetStatement { name, ty: _, value } = let_stmt;

        let dst = self.builder.alloc();

        if let Some(value) = value {
            let value = self.lower_expression(value);
            self.builder.assign(dst, value);
        }

        self.symbols.insert(&name, Variable::new(dst));
    }

    fn lower_item_stmt(&mut self, item: ItemStatement) {
        match item {
            ItemStatement::Function(fn_item) => {
                self.lower_function_item(fn_item);
            }
            _ => unimplemented!("statement {:?}", item),
        }
    }

    fn lower_if_stmt(&mut self, if_stmt: IfStatement) {
        let IfStatement {
            condition,
            then_branch,
            else_branch,
        } = if_stmt;

        let merge_blk = self.create_block("if_merge");
        let then_blk = self.create_block("if_then");
        let else_blk = else_branch.as_ref().map(|_| self.create_block("if_else"));

        let cond = self.lower_expression(condition);
        self.builder
            .br_if(cond, then_blk, else_blk.unwrap_or(merge_blk));

        self.builder.switch_to_block(then_blk);
        self.lower_block(then_branch);
        self.builder.br(merge_blk);

        if let Some(block) = else_branch {
            let else_blk = else_blk.unwrap();
            self.builder.switch_to_block(else_blk);
            self.lower_block(block);
            self.builder.br(merge_blk);
        }

        self.builder.switch_to_block(merge_blk);
    }

    fn lower_pattern(&mut self, pat: Pattern, value: Value) {
        match pat {
            Pattern::Wildcard => {}
            Pattern::Identifier(ident) => {
                let dst = self.builder.alloc();
                self.builder.assign(dst, value);
                self.symbols.insert(ident.name(), Variable::new(dst));
            }

            Pattern::Tuple(pats) => {
                for (i, pat) in pats.iter().enumerate() {
                    let index = Value::Primitive(Primitive::Integer(i as i64));
                    let field = self.builder.index_get(value, index);
                    self.lower_pattern(pat.clone(), field);
                }
            }
            _ => {
                unimplemented!("Unsupport pattern");
            }
        }
    }

    fn lower_for_stmt(&mut self, for_stmt: ForStatement) {
        let ForStatement {
            pat,
            iterable,
            body,
        } = for_stmt;

        let loop_init = self.create_block("loop_init");
        let loop_header = self.create_block("loop_header");
        let loop_body = self.create_block("iterate");
        let after_blk = self.create_block(None);

        self.enter_loop_context(after_blk, loop_header);

        self.builder.br(loop_init);

        // loop init, create iterator
        self.builder.switch_to_block(loop_init);
        let iterable = self.lower_expression(iterable);
        let iterable = self.builder.make_iterator(iterable);
        self.builder.br(loop_header);

        // loop header, check if iterator has next
        self.builder.switch_to_block(loop_header);
        let next = self.builder.iterate_next(iterable);
        let has_next = self.builder.call_property(next, "is_some", vec![]);
        self.builder.br_if(has_next, loop_body, after_blk);

        // loop body, get next value
        self.builder.switch_to_block(loop_body);

        self.symbols.enter_scope();
        let next = self.builder.call_property(next, "unwrap", vec![]);
        self.lower_pattern(pat, next);

        self.lower_block(body);

        self.symbols.leave_scope();

        self.builder.br(loop_header);

        // done loop
        self.leave_loop_context();
        self.builder.switch_to_block(after_blk);
    }

    fn lower_loop_stmt(&mut self, loop_stmt: LoopStatement) {
        let LoopStatement { body } = loop_stmt;

        let loop_body = self.create_block("loop_body");
        let after_blk = self.create_block(None);

        self.enter_loop_context(after_blk, loop_body);

        self.builder.br(loop_body);
        self.builder.switch_to_block(loop_body);

        self.lower_block(body);

        self.builder.br(loop_body);

        // done loop
        self.leave_loop_context();
        self.builder.switch_to_block(after_blk);
    }

    fn lower_while_stmt(&mut self, while_stmt: WhileStatement) {
        let WhileStatement { condition, body } = while_stmt;

        let cond_blk = self.create_block("while_condition");
        let body_blk = self.create_block("while_body");
        let after_blk = self.create_block(None);

        self.enter_loop_context(after_blk, body_blk);

        self.builder.br(cond_blk);
        self.builder.switch_to_block(cond_blk);

        let cond = self.lower_expression(condition);
        self.builder.br_if(cond, body_blk, after_blk);

        self.builder.switch_to_block(body_blk);

        self.lower_block(body);

        self.builder.br(cond_blk);

        self.builder.switch_to_block(after_blk);
    }

    fn lower_break_stmt(&mut self) {
        self.builder.br(self.loop_context().break_point);
    }

    fn lower_continue_stmt(&mut self) {
        self.builder.br(self.loop_context().continue_point);
    }

    fn lower_block(&mut self, block: BlockStatement) {
        self.symbols.enter_scope();

        for statement in block.0 {
            self.lower_statement(statement);
        }

        self.symbols.leave_scope();
    }

    fn lower_function_item(&mut self, fn_item: FunctionItem) -> Value {
        let FunctionItem {
            name, params, body, ..
        } = fn_item;

        let value = self.lower_function(Some(name.to_string()), params, body);
        self.symbols.insert(name, Variable::new(value));
        value
    }

    fn lower_function(
        &mut self,
        name: Option<String>,
        params: Vec<FunctionParam>,
        body: BlockStatement,
    ) -> Value {
        let curr = self.builder.current_block();

        let func_sig = FuncSignature::new(
            name.clone(),
            params
                .iter()
                .map(|p| FuncParam::new(p.name.clone()))
                .collect(),
        );
        let func_id = self.builder.module_mut().declare_function(func_sig.clone());

        let mut func = IrFunction::new(func_id, func_sig);

        let symbols = self.symbols.clone();

        let mut func_builder = FunctionBuilder::new(self.builder.module_mut(), &mut func);

        let mut func_lower = ASTLower::new(&mut func_builder, symbols, self.env, self.type_cx);

        let entry = func_lower.create_block(name);
        func_lower.builder.set_entry(entry);
        func_lower.builder.switch_to_block(entry);

        for (idx, param) in params.iter().enumerate() {
            let arg = func_lower.builder.load_arg(idx);

            func_lower
                .symbols
                .insert(param.name.as_str(), Variable::new(arg));
        }

        func_lower.lower_block(body);

        // append return instruction
        func_lower.builder.return_(None);

        self.builder.module_mut().define_function(func_id, func);

        self.builder.switch_to_block(curr);

        Value::Function(func_id)
    }

    fn lower_expression(&mut self, expr: ExpressionNode) -> Value {
        match expr.node {
            Expression::Literal(literal) => self.lower_literal(literal),
            Expression::Identifier(identifier) => self.lower_identifier(identifier),
            Expression::Prefix(expr) => self.lower_unary(expr),
            Expression::Binary(expr) => self.lower_binary(expr),
            Expression::Call(call) => self.lower_call(call),
            Expression::Assign(assign) => self.lower_assign(assign),
            Expression::Closure(closure) => self.lower_closure(closure),
            Expression::Array(array) => self.lower_array(array),
            Expression::Map(map) => self.lower_map(map),
            Expression::Slice(slice) => self.lower_slice(slice),
            Expression::Await(expr) => self.lower_await(*expr),
            Expression::Environment(env) => self.lower_environment(env),
            Expression::IndexGet(expr) => self.lower_index_get(expr),
            Expression::IndexSet(expr) => self.lower_index_set(expr),
            Expression::PropertyGet(expr) => self.lower_get_property(expr),
            Expression::PropertySet(expr) => self.lower_set_property(expr),
            Expression::CallMethod(expr) => self.lower_call_method(expr),
            Expression::StructExpr(expr) => self.lower_struct_expr(expr),
            _ => unimplemented!("{:?}", expr),
        }
    }

    fn lower_environment(&mut self, env: EnvironmentExpression) -> Value {
        let EnvironmentExpression(env) = env;

        self.builder.load_external_variable(env)
    }

    fn lower_index_get(&mut self, expr: IndexGetExpression) -> Value {
        let IndexGetExpression { object, index } = expr;

        let object = self.lower_expression(*object);
        let index = self.lower_expression(*index);
        self.builder.index_get(object, index)
    }

    fn lower_index_set(&mut self, expr: IndexSetExpression) -> Value {
        let IndexSetExpression {
            object,
            index,
            value,
        } = expr;

        let value = self.lower_expression(*value);
        let object = self.lower_expression(*object);
        let index = self.lower_expression(*index);
        self.builder.index_set(object, index, value);

        value
    }

    fn lower_get_property(&mut self, expr: PropertyGetExpression) -> Value {
        let PropertyGetExpression { object, property } = expr;

        let object = self.lower_expression(*object);

        self.builder.get_property(object, &property)
    }

    fn lower_set_property(&mut self, expr: PropertySetExpression) -> Value {
        let PropertySetExpression {
            object,
            property,
            value,
        } = expr;

        let value = self.lower_expression(*value);

        let object = self.lower_expression(*object);

        self.builder.set_property(object, &property, value);

        value
    }

    fn lower_struct_expr(&mut self, expr: StructExpression) -> Value {
        let StructExpression { name, fields } = expr;

        let mut field_map = fields
            .into_iter()
            .map(|StructExprField { name, value }| {
                (name.node().clone(), self.lower_expression(value))
            })
            .collect::<HashMap<_, _>>();

        let decl = self
            .type_cx
            .get_type_def(name.node())
            .expect("struct not found");
        if let TypeDef::Struct(StructDef {
            fields: decl_fields,
            ..
        }) = decl
        {
            for name in decl_fields.keys() {
                if !field_map.contains_key(name) {
                    field_map.insert(name.clone(), Value::Primitive(Primitive::Null));
                }
            }
        }

        let field_map = field_map
            .into_iter()
            .map(|(name, value)| (self.builder.make_constant(name.into()), value))
            .collect();

        self.builder.make_struct(field_map)
    }

    fn lower_call_method(&mut self, expr: CallMethodExpression) -> Value {
        let CallMethodExpression {
            object,
            method,
            args,
        } = expr;

        let args: Vec<Value> = args
            .into_iter()
            .map(|arg| self.lower_expression(arg))
            .collect();

        let object = self.lower_expression(*object);

        self.builder.call_property(object, &method, args)
    }

    fn lower_call(&mut self, expr: CallExpression) -> Value {
        let CallExpression { func, args } = expr;

        let args: Vec<Value> = args
            .into_iter()
            .map(|arg| self.lower_expression(arg))
            .collect();

        match func.node {
            Expression::Identifier(ident) => {
                match self.builder.module().find_function(ident.name()) {
                    Some(func) => self.builder.call_function(func.id, args),
                    None => match self.symbols.lookup(ident.name()) {
                        Some(var) => self.builder.make_call(var.0, args),
                        None => match self.env.get(ident.name()) {
                            Some(EnvVariable::Function(_)) => {
                                let callable = self
                                    .builder
                                    .load_external_variable(ident.name().to_string());
                                self.builder.make_call_native(callable, args)
                            }
                            _ => {
                                panic!("unknown identifier: {}", ident.name());
                            }
                        },
                    },
                }
            }
            _ => {
                unreachable!("call expression must be a member expression or identifier expression")
            }
        }
    }

    fn lower_assign(&mut self, expr: AssignExpression) -> Value {
        let AssignExpression { object, value } = expr;

        let value = self.lower_expression(*value);

        let object = self.lower_expression(*object);
        self.builder.assign(object, value);
        value
    }

    fn lower_closure(&mut self, expr: ClosureExpression) -> Value {
        let ClosureExpression { params, body } = expr;

        if let Value::Function(func_id) = self.lower_function(None, params, body) {
            return Value::Function(func_id);
        }

        unreachable!("closure expression must be a function")
    }

    fn lower_array(&mut self, expr: ArrayExpression) -> Value {
        let ArrayExpression(elements) = expr;
        let array = self.builder.make_array();

        for element in elements {
            let elem = self.lower_expression(element);
            self.builder.array_push(array, elem);
        }

        array
    }

    fn lower_map(&mut self, expr: MapExpression) -> Value {
        let MapExpression(elements) = expr;
        let map = self.builder.make_map();

        for (key, value) in elements {
            let key = self.lower_literal(key.node.as_literal().unwrap());
            let elem = self.lower_expression(value);
            self.builder.index_set(map, key, elem);
        }

        map
    }

    fn lower_await(&mut self, expr: ExpressionNode) -> Value {
        let promise = self.lower_expression(expr);

        self.builder.await_promise(promise)
    }

    fn lower_range(&mut self, expr: RangeExpression) -> Value {
        let RangeExpression { begin, end, op } = expr;

        let begin = begin.map(|expr| self.lower_expression(*expr));
        let end = end.map(|expr| self.lower_expression(*expr));

        let op = match op {
            BinOp::Range => Opcode::Range,
            BinOp::RangeInclusive => Opcode::RangeInclusive,
            _ => unreachable!("invalid range op"),
        };

        self.builder.make_range(op, begin, end)
    }

    fn lower_slice(&mut self, expr: SliceExpression) -> Value {
        let SliceExpression { object, range } = expr;

        let range = self.lower_range(range.node);

        let object = self.lower_expression(*object);

        self.builder.make_slice(object, range)
    }

    fn lower_literal(&mut self, literal: LiteralExpression) -> Value {
        match literal {
            LiteralExpression::Null => Primitive::Null.into(),
            LiteralExpression::Boolean(b) => Primitive::Boolean(b).into(),
            LiteralExpression::Integer(i) => Primitive::Integer(i).into(),
            LiteralExpression::Float(f) => Primitive::Float(f).into(),
            LiteralExpression::Char(c) => Primitive::Char(c).into(),
            LiteralExpression::String(s) => self.builder.load_constant(s.into()),
        }
    }

    fn lower_identifier(&mut self, identifier: IdentifierExpression) -> Value {
        match self.symbols.lookup(identifier.name()) {
            Some(Variable(addr)) => *addr,
            None => {
                if let Some(_env) = self.env.get(identifier.name()) {
                    return self
                        .builder
                        .load_external_variable(identifier.name().to_string());
                }
                panic!("Undefined identifier: {}", identifier.name())
            }
        }
    }

    fn lower_unary(&mut self, expr: PrefixExpression) -> Value {
        let PrefixExpression { op, rhs } = expr;

        let rhs = self.lower_expression(*rhs);

        match op {
            PrefixOp::Not => self.builder.unaryop(Opcode::Not, rhs),
            PrefixOp::Neg => self.builder.unaryop(Opcode::Neg, rhs),
        }
    }

    fn lower_binary(&mut self, expr: BinaryExpression) -> Value {
        let BinaryExpression { op, lhs, rhs } = expr;

        let lhs = self.lower_expression(*lhs);
        let rhs = self.lower_expression(*rhs);

        match op {
            BinOp::Add => self.builder.binop(Opcode::Addx, lhs, rhs),
            BinOp::Sub => self.builder.binop(Opcode::Subx, lhs, rhs),
            BinOp::Mul => self.builder.binop(Opcode::Mulx, lhs, rhs),
            BinOp::Div => self.builder.binop(Opcode::Divx, lhs, rhs),
            BinOp::Rem => self.builder.binop(Opcode::Remx, lhs, rhs),

            BinOp::Equal => self.builder.binop(Opcode::Equal, lhs, rhs),
            BinOp::NotEqual => self.builder.binop(Opcode::NotEqual, lhs, rhs),
            BinOp::Greater => self.builder.binop(Opcode::Greater, lhs, rhs),
            BinOp::GreaterEqual => self.builder.binop(Opcode::GreaterEqual, lhs, rhs),
            BinOp::Less => self.builder.binop(Opcode::Less, lhs, rhs),
            BinOp::LessEqual => self.builder.binop(Opcode::LessEqual, lhs, rhs),

            BinOp::LogicAnd => self.builder.binop(Opcode::And, lhs, rhs),
            BinOp::LogicOr => self.builder.binop(Opcode::Or, lhs, rhs),

            BinOp::Range => self.builder.make_range(Opcode::Range, Some(lhs), Some(rhs)),
            BinOp::RangeInclusive => {
                self.builder
                    .make_range(Opcode::RangeInclusive, Some(lhs), Some(rhs))
            }

            _ => unimplemented!("{:?}", op),
        }
    }

    fn declare_function(&mut self, func: &FunctionDef) -> FunctionId {
        let FunctionDef { name, params, .. } = func;

        let func_sig = FuncSignature::new(
            name.clone(),
            params
                .iter()
                .map(|(name, _ty)| FuncParam::new(name.clone()))
                .collect(),
        );
        self.builder.module_mut().declare_function(func_sig.clone())
    }

    fn create_block(&mut self, label: impl Into<Name>) -> BlockId {
        self.builder.create_block(label.into())
    }

    fn loop_context(&self) -> &LoopContext {
        self.loop_contexts.last().expect("not in loop context")
    }

    fn enter_loop_context(&mut self, break_point: BlockId, continue_point: BlockId) {
        self.loop_contexts
            .push(LoopContext::new(break_point, continue_point));
    }

    fn leave_loop_context(&mut self) {
        self.loop_contexts.pop().expect("not in loop context");
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Variable(Value);

impl Variable {
    pub fn new(addr: Value) -> Variable {
        Variable(addr)
    }
}

// #[derive(Debug, Clone)]
// pub struct SymbolNode {
//     parent: Option<SymbolTable>,
//     symbols: BTreeMap<String, Variable>,
// }

// #[derive(Debug, Clone)]
// pub struct SymbolTable(Rc<RefCell<SymbolNode>>);

// impl SymbolTable {
//     pub fn new() -> Self {
//         SymbolTable(Rc::new(RefCell::new(SymbolNode {
//             parent: None,
//             symbols: BTreeMap::new(),
//         })))
//     }

//     fn get(&self, name: &str) -> Option<Variable> {
//         if let Some(value) = self.0.borrow().symbols.get(name) {
//             return Some(*value);
//         }
//         if let Some(parent) = &self.0.borrow().parent {
//             return parent.get(name);
//         }
//         None
//     }

//     fn insert(&mut self, name: impl Into<String>, value: Variable) {
//         self.0.borrow_mut().symbols.insert(name.into(), value);
//     }

//     fn new_scope(&self) -> SymbolTable {
//         SymbolTable(Rc::new(RefCell::new(SymbolNode {
//             parent: Some(self.clone()),
//             symbols: BTreeMap::new(),
//         })))
//     }
// }
