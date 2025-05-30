use std::{collections::HashMap, default};

use crate::{Environment, compiler::symbol::SymbolTable};

use super::ast::{syntax::*, walker::Walker};

#[derive(Debug, Clone)]
pub struct TypeError {
    pub span: Span,
    pub kind: ErrKind,
}

impl TypeError {
    pub fn new(span: Span, kind: ErrKind) -> TypeError {
        TypeError { span, kind }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

impl From<ErrKind> for TypeError {
    fn from(value: ErrKind) -> Self {
        TypeError {
            span: Span::new(0, 0),
            kind: value,
        }
    }
}

#[derive(Debug, Clone)]
enum ErrKind {
    Message(String),
    UnresovledType(String),
    DuplicateName(String),
    TypeMismatch { expected: Type, actual: Type },
}

impl ErrKind {
    pub fn with_span(self, span: Span) -> TypeError {
        TypeError { span, kind: self }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeId(usize);

#[derive(Debug, Clone, PartialEq, Default)]
pub enum Type {
    Boolean,
    Byte,
    Integer,
    Float,
    Char,
    String,
    Array,
    Tuple,
    Enum(TypeId),
    Struct(TypeId),
    Function(Box<FunctionDef>),
    Any,
    #[default]
    Unknown,
}

impl Type {
    pub fn is_any(&self) -> bool {
        matches!(self, Type::Any)
    }
    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Boolean)
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Byte | Type::Integer | Type::Float)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Type::String)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef {
    Struct(StructDef),
    Enum(EnumDef),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<(String, Option<Type>)>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<(String, Option<Type>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub fields: HashMap<String, Type>,
}

pub struct TypeContext {
    type_defs: HashMap<TypeId, TypeDef>,
    name_to_id: HashMap<String, TypeId>,

    functions: HashMap<String, Box<FunctionDef>>,

    next_id: usize,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            type_defs: HashMap::new(),
            name_to_id: HashMap::new(),
            functions: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn functions(&self) -> impl Iterator<Item = &FunctionDef> {
        self.functions.values().map(|func| &**func)
    }

    pub fn type_is_defined(&self, name: &str) -> bool {
        self.name_to_id.contains_key(name) || self.functions.contains_key(name)
    }

    pub fn get_type_id(&self, name: &str) -> Option<TypeId> {
        self.name_to_id.get(name).copied()
    }

    pub fn get_type(&self, name: &str) -> Option<Type> {
        match self.name_to_id.get(name) {
            Some(id) => match self.type_defs.get(id) {
                Some(TypeDef::Struct(_)) => Some(Type::Struct(*id)),
                Some(TypeDef::Enum(_)) => Some(Type::Enum(*id)),
                None => panic!("Type not found"),
            },
            None => self
                .functions
                .get(name)
                .map(|func| Type::Function(func.clone())),
        }
    }

    pub fn get_type_def(&self, name: &str) -> Option<&TypeDef> {
        match self.name_to_id.get(name) {
            Some(id) => self.type_defs.get(id),
            None => None,
        }
    }

    pub fn get_function_def(&self, name: &str) -> Option<&FunctionDef> {
        self.functions.get(name).map(|v| &**v)
    }

    pub fn analyze_type_def(&mut self, stmts: &[StatementNode]) -> Result<(), TypeError> {
        // round 1, decl types
        for stmt in stmts {
            match &stmt.node {
                Statement::Item(ItemStatement::Struct(StructItem { name, .. })) => {
                    self.decl_type(
                        name.clone(),
                        TypeDef::Struct(StructDef {
                            name: name.clone(),
                            fields: HashMap::new(),
                        }),
                    )
                    .map_err(|err| err.with_span(stmt.span))?;
                }
                Statement::Item(ItemStatement::Enum(item)) => {
                    self.decl_type(
                        item.name.clone(),
                        TypeDef::Enum(EnumDef {
                            name: item.name.clone(),
                            variants: Vec::new(),
                        }),
                    );
                }
                _ => {}
            }
        }

        // round 2, resolve types
        for stmt in stmts {
            match &stmt.node {
                Statement::Item(ItemStatement::Function(func)) => {}
                Statement::Item(ItemStatement::Struct(item)) => {
                    self.analyze_struct_item(item)?;
                }
                Statement::Item(ItemStatement::Enum(item)) => {
                    self.analyze_enum_item(item)?;
                }
                _ => {
                    continue;
                }
            }
        }

        Ok(())
    }

    fn analyze_function_item(&mut self, item: &FunctionItem) -> Result<(), TypeError> {
        let FunctionItem {
            name,
            params,
            return_ty,
            ..
        } = item;

        let mut func = FunctionDef {
            name: name.clone(),
            params: Vec::new(),
            return_type: return_ty
                .as_ref()
                .map(|ty| self.resolve_type(ty))
                .transpose()?,
        };

        for param in params {
            let param_type = param
                .ty
                .as_ref()
                .map(|ty| self.resolve_type(&ty))
                .transpose()?;
            func.params.push((param.name.clone(), param_type));
        }

        self.functions.insert(name.clone(), Box::new(func));

        Ok(())
    }

    fn analyze_struct_item(&mut self, item: &StructItem) -> Result<TypeId, TypeError> {
        let StructItem { name, fields } = item;

        let type_id = self.name_to_id.get(name).unwrap();

        let fields: HashMap<String, Type> = fields
            .iter()
            .map(|field| {
                self.resolve_type(&field.ty)
                    .map(|ty| (field.name.clone(), ty))
            })
            .collect::<Result<_, TypeError>>()?;

        match self.type_defs.get_mut(type_id) {
            Some(TypeDef::Struct(struct_def)) => {
                // update the struct definition
                struct_def.fields = fields;
            }
            _ => {
                return Err(ErrKind::Message(format!("Type {} is not a struct", name)).into());
            }
        }

        Ok(*type_id)
    }

    fn analyze_enum_item(&mut self, item: &EnumItem) -> Result<TypeId, TypeError> {
        let EnumItem { name, variants } = item;

        let type_id = self.name_to_id.get(name).unwrap();

        let mut enum_variants = Vec::new();

        for variant in variants {
            let EnumVariant { name, variant } = variant;

            let variant_type = variant
                .as_ref()
                .map(|ty| self.resolve_type(&ty))
                .transpose()?;

            enum_variants.push((name.to_string(), variant_type));
        }

        match self.type_defs.get_mut(type_id) {
            Some(TypeDef::Enum(enum_def)) => {
                // update the enum definition
                enum_def.variants = enum_variants;
            }
            _ => {
                return Err(ErrKind::Message(format!("Type {} is not an enum", name)).into());
            }
        }

        Ok(*type_id)
    }

    fn decl_type(&mut self, name: String, ty: TypeDef) -> Result<TypeId, TypeError> {
        if self.name_to_id.contains_key(&name) {
            return Err(ErrKind::DuplicateName(name).into());
        }

        let id = self.next_id();
        self.type_defs.insert(id, ty);
        self.name_to_id.insert(name, id);

        Ok(id)
    }

    fn next_id(&mut self) -> TypeId {
        let id = self.next_id;
        self.next_id += 1;
        TypeId(id)
    }

    /// Try to analyze type expressions when possible, otherwise return Type::Unknown.
    fn try_resolve_type(&self, type_expr: &TypeExpression) -> Type {
        match type_expr {
            TypeExpression::Any => Type::Any,
            TypeExpression::Boolean => Type::Boolean,
            TypeExpression::Byte => Type::Byte,
            TypeExpression::Integer => Type::Integer,
            TypeExpression::Float => Type::Float,
            TypeExpression::Char => Type::Char,
            TypeExpression::String => Type::String,
            TypeExpression::Array(_) => Type::Array,
            TypeExpression::Tuple(_) => Type::Tuple,
            TypeExpression::UserDefined(name) => match self.name_to_id.get(name).cloned() {
                Some(id) => match self.type_defs.get(&id) {
                    Some(TypeDef::Struct(_)) => Type::Struct(id),
                    Some(TypeDef::Enum(_)) => Type::Enum(id),
                    _ => panic!("Invalid type"),
                },
                None => Type::Unknown,
            },
            _ => Type::Unknown,
        }
    }

    pub fn resolve_type(&self, type_expr: &TypeExpression) -> Result<Type, TypeError> {
        match self.try_resolve_type(type_expr) {
            Type::Unknown => Err(ErrKind::UnresovledType(format!("{:?}", type_expr)).into()),
            ty => Ok(ty),
        }
    }
}

pub struct TypeChecker<'a> {
    type_cx: &'a TypeContext,
    current_function_return_type: Option<Type>,
    symbols: SymbolTable<Type>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(type_cx: &'a TypeContext) -> Self {
        TypeChecker {
            type_cx,
            current_function_return_type: None,
            symbols: SymbolTable::new(),
        }
    }

    pub fn check_program(&mut self, program: &Program, env: &Environment) -> Result<(), TypeError> {
        // all env is any
        for name in env.keys() {
            self.symbols.insert(name.to_string(), Type::Any);
        }
        // insert function type
        for (name, func) in self.type_cx.functions.iter() {
            self.symbols
                .insert(func.name.clone(), Type::Function(func.clone()));
        }

        for item in &program.stmts {
            self.check_statement(&item)?;
        }
        Ok(())
    }

    fn check_statement(&mut self, stmt: &StatementNode) -> Result<(), TypeError> {
        match &stmt.node {
            Statement::Let(let_stmt) => self.check_let_statement(let_stmt),
            Statement::Block(block) => self.check_block_statement(block),
            Statement::If(if_stmt) => self.check_if_statement(if_stmt),
            Statement::While(while_stmt) => self.check_while_statement(while_stmt),
            Statement::For(for_stmt) => self.check_for_statement(for_stmt),
            Statement::Loop(loop_stmt) => self.check_loop_statement(loop_stmt),
            Statement::Return(return_stmt) => self.check_return_statement(return_stmt),
            Statement::Expression(expr) => self.analyze_expression(expr).map(|_| ()),
            Statement::Empty => Ok(()),
            Statement::Break => Ok(()),
            Statement::Continue => Ok(()),
            Statement::Item(item_stmt) => self.check_item_statement(item_stmt),
        }
    }

    // 新增方法：检查块语句
    fn check_block_statement(&mut self, block: &BlockStatement) -> Result<(), TypeError> {
        self.symbols.enter_scope();

        for stmt in &block.0 {
            self.check_statement(stmt)?;
        }

        self.symbols.leave_scope();
        Ok(())
    }

    // 新增方法：检查条件语句
    fn check_if_statement(&mut self, if_stmt: &IfStatement) -> Result<(), TypeError> {
        let condition_type = self.analyze_expression(&if_stmt.condition)?;

        if condition_type != Type::Boolean && condition_type != Type::Any {
            return Err(ErrKind::TypeMismatch {
                expected: Type::Boolean,
                actual: condition_type,
            }
            .with_span(if_stmt.condition.span));
        }

        self.check_block_statement(&if_stmt.then_branch)?;
        if let Some(else_branch) = &if_stmt.else_branch {
            self.check_block_statement(else_branch)?;
        }
        Ok(())
    }

    // 新增方法：检查循环语句
    fn check_while_statement(&mut self, while_stmt: &WhileStatement) -> Result<(), TypeError> {
        let condition_type = self.analyze_expression(&while_stmt.condition)?;

        if condition_type != Type::Boolean && condition_type != Type::Any {
            return Err(ErrKind::TypeMismatch {
                expected: Type::Boolean,
                actual: condition_type,
            }
            .with_span(while_stmt.condition.span));
        }

        self.check_block_statement(&while_stmt.body)?;
        Ok(())
    }

    // 新增方法：检查 for 循环语句
    fn check_for_statement(&mut self, for_stmt: &ForStatement) -> Result<(), TypeError> {
        self.analyze_expression(&for_stmt.iterable)?;
        self.check_block_statement(&for_stmt.body)?;
        Ok(())
    }

    // 新增方法：检查无限循环语句
    fn check_loop_statement(&mut self, loop_stmt: &LoopStatement) -> Result<(), TypeError> {
        self.check_block_statement(&loop_stmt.body)?;
        Ok(())
    }

    // 新增方法：检查返回语句
    fn check_return_statement(&mut self, return_stmt: &ReturnStatement) -> Result<(), TypeError> {
        if let Some(expr) = &return_stmt.value {
            let return_ty = self.analyze_expression(expr)?;

            if let Some(expected_ty) = &self.current_function_return_type {
                if return_ty != *expected_ty {
                    return Err(ErrKind::TypeMismatch {
                        expected: expected_ty.clone(),
                        actual: return_ty,
                    }
                    .with_span(expr.span()));
                }
            }
        }
        Ok(())
    }

    // 新增方法：检查项语句
    fn check_item_statement(&mut self, item_stmt: &ItemStatement) -> Result<(), TypeError> {
        if let ItemStatement::Function(func) = item_stmt {
            self.check_function_item(func)?;
        }

        Ok(())
    }

    fn check_function_item(&mut self, func_item: &FunctionItem) -> Result<(), TypeError> {
        // new scope
        let old_return_type = self.current_function_return_type.clone();
        self.symbols.enter_scope();

        for param in &func_item.params {
            let param_type = match param.ty.as_ref() {
                Some(ty) => self.type_cx.resolve_type(&ty)?,
                None => Type::Any,
            };

            self.symbols.insert(param.name.clone(), param_type);
        }

        self.current_function_return_type = func_item
            .return_ty
            .as_ref()
            .map(|ty| self.type_cx.resolve_type(ty))
            .transpose()?;

        self.check_block_statement(&func_item.body)?;

        // restore
        self.symbols.leave_scope();
        self.current_function_return_type = old_return_type;

        Ok(())
    }

    fn check_let_statement(&mut self, let_stmt: &LetStatement) -> Result<(), TypeError> {
        let ty = let_stmt
            .ty
            .as_ref()
            .map(|ty| self.type_cx.resolve_type(&ty))
            .transpose()?;

        if let Some(expr) = let_stmt.value.as_ref() {
            let value_type = self.analyze_expression(expr)?;
            if ty.is_some() && (ty.as_ref() != Some(&value_type)) {
                return Err(ErrKind::TypeMismatch {
                    expected: ty.unwrap(),
                    actual: value_type,
                }
                .with_span(expr.span));
            }
        }

        Ok(())
    }

    fn analyze_expression(&mut self, expr: &ExpressionNode) -> Result<Type, TypeError> {
        let ret = match &expr.node {
            Expression::Literal(lit) => self.analyze_literal(lit),
            Expression::Identifier(id) => self.analyze_identifier(id),
            Expression::Binary(bin) => self.analyze_binary(bin),
            Expression::Prefix(prefix) => self.analyze_prefix(prefix),
            Expression::Call(call) => self.analyze_call(call),
            Expression::Environment(env) => Ok(Type::String),
            Expression::Path(path) => self.analyze_path(path),
            Expression::Tuple(tuple) => self.analyze_tuple(tuple),
            Expression::Array(arr) => self.analyze_array(arr),
            Expression::Map(map) => Ok(Type::Any), // 暂定Map类型为Any
            Expression::Closure(closure) => self.analyze_closure(closure),
            Expression::Range(range) => Ok(Type::Any), // 暂定Range类型为Any
            Expression::Slice(slice) => self.analyze_slice(slice),
            Expression::Assign(assign) => self.analyze_assign(assign),
            Expression::IndexGet(index) => self.analyze_index_get(index),
            Expression::IndexSet(index) => self.analyze_index_set(index),
            Expression::PropertyGet(prop) => self.analyze_property_get(prop),
            Expression::PropertySet(prop) => self.analyze_property_set(prop),
            Expression::CallMethod(call) => self.analyze_call_method(call),
            Expression::StructExpr(struct_) => self.analyze_struct_expr(struct_),
            Expression::Await(expr) => self.analyze_expression(expr),
            Expression::Try(expr) => self.analyze_expression(expr),
            // _ => Err(ErrKind::Message(format!("Unsupported expression: {:?}", expr.node)).into()),
        };

        ret.map_err(|err| {
            if !err.span.is_empty() {
                return err.with_span(expr.span);
            } else {
                err
            }
        })
    }

    fn analyze_path(&self, path: &PathExpression) -> Result<Type, TypeError> {
        // 路径表达式类型解析逻辑
        Ok(Type::Any) // 暂定返回Any类型
    }

    fn analyze_tuple(&mut self, tuple: &TupleExpression) -> Result<Type, TypeError> {
        // 元组类型解析逻辑
        Ok(Type::Tuple)
    }

    fn analyze_array(&mut self, arr: &ArrayExpression) -> Result<Type, TypeError> {
        // 数组类型解析逻辑
        Ok(Type::Array)
    }

    fn analyze_closure(&mut self, closure: &ClosureExpression) -> Result<Type, TypeError> {
        // // 闭包类型解析逻辑
        // Ok(Type::Function(Box::new(FunctionDef {
        //     name: "".to_string(),
        //     params: vec![],
        //     return_type: None,
        // })))

        Ok(Type::Any) // 临时返回 Any 类型
    }

    fn analyze_slice(&mut self, slice: &SliceExpression) -> Result<Type, TypeError> {
        // 切片类型解析逻辑
        Ok(Type::Array)
    }

    fn analyze_assign(&mut self, assign: &AssignExpression) -> Result<Type, TypeError> {
        // 赋值表达式类型解析逻辑
        self.analyze_expression(&assign.value)
    }

    fn analyze_index_get(&mut self, index: &IndexGetExpression) -> Result<Type, TypeError> {
        // 索引获取类型解析逻辑
        Ok(Type::Any) // 暂定返回Any类型
    }

    fn analyze_index_set(&mut self, index: &IndexSetExpression) -> Result<Type, TypeError> {
        // 索引设置类型解析逻辑
        self.analyze_expression(&index.value)
    }

    fn analyze_property_get(&mut self, prop: &PropertyGetExpression) -> Result<Type, TypeError> {
        // 属性获取类型解析逻辑
        Ok(Type::Any) // 暂定返回Any类型
    }

    fn analyze_property_set(&mut self, prop: &PropertySetExpression) -> Result<Type, TypeError> {
        // 属性设置类型解析逻辑
        self.analyze_expression(&prop.value)
    }

    fn analyze_call_method(&mut self, call: &CallMethodExpression) -> Result<Type, TypeError> {
        // 调用方法类型解析逻辑
        Ok(Type::Any) // 暂定返回Any类型
    }

    fn analyze_struct_expr(&mut self, struct_expr: &StructExpression) -> Result<Type, TypeError> {
        match self.type_cx.get_type_def(&struct_expr.name.node()) {
            Some(TypeDef::Struct(struct_def)) => {
                for field in &struct_expr.fields {
                    let field_type = self.analyze_expression(&field.value)?;
                    match struct_def.fields.get(&field.name.node) {
                        Some(expected_type) => {
                            if field_type != *expected_type {
                                return Err(TypeError::new(
                                    field.value.span(),
                                    ErrKind::Message(format!(
                                        "Expected type {:?} for field {:?}, found {:?}",
                                        expected_type, field.name, field_type
                                    )),
                                ));
                            }
                        }
                        None => {}
                    }
                }

                Ok(self.type_cx.get_type(&struct_expr.name.node()).unwrap())
            }
            Some(ty) => Err(TypeError::new(
                struct_expr.name.span(),
                ErrKind::Message(format!("Expected struct type, found {:?}", ty)),
            )),
            None => Err(ErrKind::UnresovledType(struct_expr.name.node().clone()).into()),
        }
    }

    fn analyze_literal(&self, lit: &LiteralExpression) -> Result<Type, TypeError> {
        match lit {
            LiteralExpression::Null => Ok(Type::Any),
            LiteralExpression::Boolean(_) => Ok(Type::Boolean),
            LiteralExpression::Integer(_) => Ok(Type::Integer),
            LiteralExpression::Float(_) => Ok(Type::Float),
            LiteralExpression::Char(_) => Ok(Type::Char),
            LiteralExpression::String(_) => Ok(Type::String),
        }
    }

    fn analyze_identifier(&self, id: &IdentifierExpression) -> Result<Type, TypeError> {
        match self.symbols.lookup(&id.0) {
            Some(ty) => Ok(ty.clone()),
            None => Err(ErrKind::Message(format!("Undefined identifier: {}", id.0)).into()),
        }
    }

    fn analyze_binary(&mut self, bin: &BinaryExpression) -> Result<Type, TypeError> {
        let lhs_type = self.analyze_expression(&bin.lhs)?;
        let rhs_type = self.analyze_expression(&bin.rhs)?;

        // Handle Type::Any as compatible with any type
        if lhs_type == Type::Any || rhs_type == Type::Any {
            return Ok(Type::Any);
        }

        match bin.op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                if lhs_type.is_numeric() && lhs_type == rhs_type {
                    Ok(lhs_type) // Simplified for demonstration
                } else {
                    Err(ErrKind::Message(format!(
                        "Type mismatch in binary operation: {:?} and {:?}",
                        lhs_type, rhs_type
                    ))
                    .into())
                }
            }
            BinOp::Equal | BinOp::NotEqual => {
                if lhs_type == rhs_type {
                    Ok(Type::Boolean)
                } else {
                    Err(ErrKind::Message(format!(
                        "Type mismatch in comparison: {:?} and {:?}",
                        lhs_type, rhs_type
                    ))
                    .into())
                }
            }
            _ => Err(ErrKind::Message(format!("Unsupported binary operator: {:?}", bin.op)).into()),
        }
    }

    fn analyze_prefix(&mut self, prefix: &PrefixExpression) -> Result<Type, TypeError> {
        let rhs_type = self.analyze_expression(&prefix.rhs)?;

        // Handle Type::Any as compatible with any type
        if rhs_type == Type::Any {
            return Ok(Type::Any);
        }

        match prefix.op {
            PrefixOp::Neg => {
                if rhs_type.is_numeric() {
                    Ok(rhs_type)
                } else {
                    Err(ErrKind::Message(format!(
                        "Cannot apply negation to non-numeric type: {:?}",
                        rhs_type
                    ))
                    .into())
                }
            }
            PrefixOp::Not => {
                if rhs_type == Type::Boolean {
                    Ok(Type::Boolean)
                } else {
                    Err(ErrKind::Message(format!(
                        "Cannot apply logical NOT to non-boolean type: {:?}",
                        rhs_type
                    ))
                    .into())
                }
            }
        }
    }

    fn analyze_call(&mut self, call: &CallExpression) -> Result<Type, TypeError> {
        let func_type = self.analyze_expression(&call.func)?;

        // Handle Type::Any as compatible with any type
        if func_type == Type::Any {
            return Ok(Type::Any);
        }

        match func_type {
            Type::Function(func_def) => {
                // Check the number of arguments
                if func_def.params.len() != call.args.len() {
                    return Err(ErrKind::Message(format!(
                        "Expected {} arguments, but got {}",
                        func_def.params.len(),
                        call.args.len()
                    ))
                    .into());
                }

                // Check each argument type
                for (i, (param_name, param_type)) in func_def.params.iter().enumerate() {
                    let arg_type = self.analyze_expression(&call.args[i])?;
                    if let Some(expected_type) = param_type {
                        // Handle Type::Any as compatible with any type
                        if *expected_type != Type::Any && arg_type != *expected_type {
                            return Err(ErrKind::Message(format!(
                                "Argument {} has type {:?}, but expected {:?}",
                                i + 1,
                                arg_type,
                                expected_type
                            ))
                            .into());
                        }
                    }
                }

                // Return the function's return type
                Ok(func_def.return_type.unwrap_or(Type::Any))
            }
            _ => Err(
                ErrKind::Message(format!("Cannot call non-function type: {:?}", func_type)).into(),
            ),
        }
    }
}
