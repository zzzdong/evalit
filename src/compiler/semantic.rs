use super::CompileError;
use super::ast::syntax::*;
use super::typing::TypeContext;
use crate::Environment;

pub struct SemanticAnalyzer<'a> {
    type_cx: &'a mut TypeContext,
    // 当前函数的返回类型，用于检查return语句
    current_function_return_type: Option<Type>,
    // 循环嵌套计数，用于检查break和continue语句
    loop_depth: usize,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(type_cx: &'a mut TypeContext) -> Self {
        SemanticAnalyzer {
            type_cx,
            current_function_return_type: None,
            loop_depth: 0,
        }
    }

    /// 对Program进行语义检查
    pub fn analyze_program(
        &mut self,
        program: &mut Program,
        env: &Environment,
    ) -> Result<(), CompileError> {
        // 第一阶段：收集环境变量
        for name in env.symbols.keys() {
            self.type_cx.set_type(name.to_string(), Type::Any);
        }

        // 第二阶段：收集声明
        self.type_cx.analyze_type_decl(&program.stmts);

        // 第三阶段：分析所有语句
        for stmt in &mut program.stmts {
            self.analyze_statement(stmt)?;
        }
        Ok(())
    }

    /// 分析语句并推断类型
    fn analyze_statement(&mut self, stmt: &mut StatementNode) -> Result<(), CompileError> {
        let span = stmt.span;

        match &mut stmt.node {
            Statement::Expression(expr) => self.analyze_expression(expr).map(|_| ()),
            Statement::Let(let_stmt) => self.analyze_let_statement(let_stmt),
            Statement::If(if_stmt) => self.analyze_if_statement(if_stmt),
            Statement::Loop(loop_stmt) => self.analyze_loop_statement(loop_stmt),
            Statement::While(while_stmt) => self.analyze_while_statement(while_stmt),
            Statement::For(for_stmt) => self.analyze_for_statement(for_stmt),
            Statement::Return(return_stmt) => self.analyze_return_statement(return_stmt, span),
            Statement::Block(block) => self.analyze_block(block),
            Statement::Break => self.analyze_break_statement(span),
            Statement::Continue => self.analyze_continue_statement(span),
            Statement::Empty => Ok(()),
            Statement::Item(ItemStatement::Fn(func)) => self.analyze_function_item(func),
            Statement::Item(ItemStatement::Struct(item)) => self.analyze_struct_item(item),
            Statement::Item(ItemStatement::Enum(EnumItem { .. })) => {
                unimplemented!("EnumItem not implemented")
            }
        }
    }

    fn analyze_let_statement(&mut self, let_stmt: &mut LetStatement) -> Result<(), CompileError> {
        let LetStatement { name, ty, value } = let_stmt;

        let decl_ty = match ty {
            Some(type_expr) => {
                let decl_type = self.type_from_type_expr(type_expr)?;
                Some(decl_type)
            }
            None => None,
        };

        let value_ty = match value {
            Some(value) => {
                let value_ty = self.analyze_expression(value)?;
                Some(value_ty)
            }
            None => None,
        };

        match (decl_ty, value_ty) {
            (Some(decl_type), Some(value_ty)) => {
                if decl_type != value_ty {
                    return Err(CompileError::type_mismatch(
                        decl_type,
                        value_ty,
                        value.clone().unwrap().span(),
                    ));
                }
                self.type_cx.set_type(name.clone(), decl_type);
            }
            (Some(decl_type), None) => {
                self.type_cx.set_type(name.clone(), decl_type);
            }
            (None, Some(value_ty)) => {
                self.type_cx.set_type(name.clone(), value_ty);
            }
            (None, None) => {
                self.type_cx.set_type(name.clone(), Type::Any);
            }
        }

        Ok(())
    }

    /// 分析If语句
    fn analyze_if_statement(&mut self, if_stmt: &mut IfStatement) -> Result<(), CompileError> {
        // 分析条件表达式
        let condition_ty = self.analyze_expression(&mut if_stmt.condition)?;

        // TODO: 条件必须是布尔类型
        if condition_ty != Type::Boolean && condition_ty != Type::Any {
            return Err(CompileError::type_mismatch(
                Type::Boolean,
                condition_ty.clone(),
                if_stmt.condition.span,
            ));
        }

        // 分析then分支
        self.analyze_block(&mut if_stmt.then_branch)?;

        // 分析else分支（如果有）
        if let Some(else_branch) = &mut if_stmt.else_branch {
            self.analyze_block(else_branch)?;
        }

        Ok(())
    }

    fn analyze_loop_statement(
        &mut self,
        loop_stmt: &mut LoopStatement,
    ) -> Result<(), CompileError> {
        // 增加循环深度
        self.loop_depth += 1;

        // 分析循环体
        self.analyze_block(&mut loop_stmt.body)?;

        // 减少循环深度
        self.loop_depth -= 1;

        Ok(())
    }

    /// 分析While语句
    fn analyze_while_statement(
        &mut self,
        while_stmt: &mut WhileStatement,
    ) -> Result<(), CompileError> {
        // 分析条件表达式
        let condition_ty = self.analyze_expression(&mut while_stmt.condition)?;

        // 条件必须是布尔类型
        if !condition_ty.is_boolean() {
            return Err(CompileError::type_mismatch(
                Type::Boolean,
                condition_ty.clone(),
                while_stmt.condition.span,
            ));
        }

        // 增加循环深度
        self.loop_depth += 1;

        // 分析循环体
        self.analyze_block(&mut while_stmt.body)?;

        // 减少循环深度
        self.loop_depth -= 1;

        Ok(())
    }

    /// 分析For语句
    fn analyze_for_statement(&mut self, for_stmt: &mut ForStatement) -> Result<(), CompileError> {
        // 创建新的作用域
        let old_env = std::mem::replace(self.type_cx, self.type_cx.clone());

        // 分析迭代器表达式
        // self.analyze_expression(&mut for_stmt.iterable)?;

        self.analyze_pattern(&mut for_stmt.pat, &mut for_stmt.iterable)?;

        // 增加循环深度
        self.loop_depth += 1;

        // 分析循环体
        self.analyze_block(&mut for_stmt.body)?;

        // 减少循环深度
        self.loop_depth -= 1;

        // 恢复作用域
        let _ = std::mem::replace(self.type_cx, old_env);

        Ok(())
    }

    fn analyze_pattern(
        &mut self,
        pattern: &mut Pattern,
        expr: &mut ExpressionNode,
    ) -> Result<(), CompileError> {
        self.analyze_expression(expr)?;

        // 检查模式是否匹配表达式
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Identifier(id) => {
                self.type_cx.set_type(id.clone(), Type::Any);
            }
            Pattern::Tuple(tuple) => {
                for pat in tuple.iter_mut() {
                    self.analyze_pattern(pat, expr)?;
                }
            }
            Pattern::Literal(_literal) => {}
        }

        Ok(())
    }

    /// 分析Break语句
    fn analyze_break_statement(&mut self, span: Span) -> Result<(), CompileError> {
        if self.loop_depth == 0 {
            return Err(CompileError::BreakOutsideLoop { span });
        }
        Ok(())
    }

    /// 分析Continue语句
    fn analyze_continue_statement(&mut self, span: Span) -> Result<(), CompileError> {
        if self.loop_depth == 0 {
            return Err(CompileError::ContinueOutsideLoop { span });
        }
        Ok(())
    }

    /// 分析Return语句
    fn analyze_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
        _span: Span,
    ) -> Result<(), CompileError> {
        if let Some(expr) = &mut return_stmt.value {
            self.analyze_expression(expr)?;
        }
        Ok(())
    }

    /// 分析代码块
    fn analyze_block(&mut self, block: &mut BlockStatement) -> Result<(), CompileError> {
        // 创建新的作用域
        let old_env = std::mem::replace(self.type_cx, self.type_cx.clone());

        // 分析块中的所有语句
        for stmt in &mut block.0 {
            self.analyze_statement(stmt)?;
        }

        // 恢复作用域
        let _ = std::mem::replace(self.type_cx, old_env);

        Ok(())
    }

    /// 分析函数定义
    fn analyze_function_item(&mut self, func: &mut FunctionItem) -> Result<(), CompileError> {
        // 创建新的作用域
        let old_env = std::mem::replace(self.type_cx, self.type_cx.clone());

        // 获取函数返回类型
        let return_ty = if let Some(ty_expr) = &func.return_ty {
            self.type_from_type_expr(ty_expr)?
        } else {
            Type::Any
        };

        // 添加参数到环境
        for param in &func.params {
            let param_ty = match &param.ty {
                Some(ty) => self.type_from_type_expr(ty)?,
                None => Type::Any,
            };

            self.type_cx.set_type(param.name.clone(), param_ty);
        }

        // 保存当前函数的返回类型，用于检查return语句
        let old_return_type = self.current_function_return_type.replace(return_ty.clone());

        // 分析函数体
        self.analyze_block(&mut func.body)?;

        // 恢复环境
        let _ = std::mem::replace(self.type_cx, old_env);
        self.current_function_return_type = old_return_type;

        Ok(())
    }

    fn analyze_struct_item(&mut self, item: &mut StructItem) -> Result<(), CompileError> {
        Ok(())
    }

    /// 分析表达式并推断类型
    fn analyze_expression(&mut self, expr: &mut ExpressionNode) -> Result<Type, CompileError> {
        let ty = match &mut expr.node {
            Expression::Literal(lit) => self.analyze_literal_expression(lit)?,
            Expression::Identifier(ident) => self.anlyze_identifier_expression(ident)?,
            Expression::Binary(expr) => self.analyze_binary_expression(expr)?,
            Expression::Prefix(expr) => self.analyze_prefix_expression(expr)?,
            Expression::Call(expr) => self.analyze_call_expression(expr)?,
            Expression::Array(expr) => self.analyze_array_expression(expr)?,
            Expression::Map(expr) => self.analyze_map_expression(expr)?,
            Expression::IndexGet(expr) => self.analyze_index_get_expression(expr)?,
            Expression::IndexSet(expr) => self.analyze_index_set_expression(expr)?,
            Expression::PropertyGet(expr) => self.analyze_property_get_expression(expr)?,
            Expression::PropertySet(expr) => self.analyze_property_set_expression(expr)?,
            Expression::Assign(expr) => self.analyze_assign_expression(expr)?,
            Expression::Range(expr) => self.analyze_range_expression(expr)?,
            Expression::Slice(expr) => self.analyze_slice_expression(expr)?,
            Expression::Try(expr) => self.analyze_try_expression(expr)?,
            Expression::Await(expr) => self.analyze_await_expression(expr)?,
            Expression::CallMethod(expr) => self.analyze_call_method_expression(expr)?,
            _ => {
                // 处理其他未实现的表达式类型
                Type::Any
            }
        };

        expr.ty = ty.clone();

        Ok(ty)
    }

    fn analyze_literal_expression(
        &mut self,
        lit: &mut LiteralExpression,
    ) -> Result<Type, CompileError> {
        Ok(match lit {
            LiteralExpression::Null => Type::Any,
            LiteralExpression::Boolean(_) => Type::Boolean,
            LiteralExpression::Integer(_) => Type::Integer,
            LiteralExpression::Float(_) => Type::Float,
            LiteralExpression::Char(_) => Type::Char,
            LiteralExpression::String(_) => Type::String,
        })
    }

    fn anlyze_identifier_expression(
        &mut self,
        ident: &mut IdentifierExpression,
    ) -> Result<Type, CompileError> {
        if let Some(ty) = self.type_cx.get_type(&ident.0) {
            return Ok(ty.clone());
        }

        Err(CompileError::UndefinedVariable {
            name: ident.0.clone(),
        })
    }

    fn analyze_binary_expression(
        &mut self,
        expr: &mut BinaryExpression,
    ) -> Result<Type, CompileError> {
        let lhs_ty = self.analyze_expression(expr.lhs.as_mut())?;
        let rhs_ty = self.analyze_expression(expr.rhs.as_mut())?;

        if lhs_ty == Type::Any || rhs_ty == Type::Any {
            return Ok(Type::Any); // Object类型可以和任何类型比较
        }

        match expr.op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                if (!lhs_ty.is_numeric() || rhs_ty != lhs_ty)
                    && lhs_ty != Type::String
                    && lhs_ty != Type::Char
                {
                    return Err(CompileError::type_mismatch(
                        Type::Integer,
                        lhs_ty,
                        expr.lhs.span(),
                    ));
                }
            }
            BinOp::LogicAnd | BinOp::LogicOr => {
                if lhs_ty != Type::Boolean || rhs_ty != Type::Boolean {
                    return Err(CompileError::type_mismatch(
                        Type::Boolean,
                        lhs_ty,
                        expr.lhs.span(),
                    ));
                }
            }
            _ => {}
        }

        Ok(match expr.op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => lhs_ty,
            BinOp::LogicAnd | BinOp::LogicOr => Type::Boolean,
            BinOp::Less
            | BinOp::LessEqual
            | BinOp::Greater
            | BinOp::GreaterEqual
            | BinOp::Equal
            | BinOp::NotEqual => Type::Boolean,
            _ => Type::Any,
        })
    }

    fn analyze_prefix_expression(
        &mut self,
        expr: &mut PrefixExpression,
    ) -> Result<Type, CompileError> {
        let rhs_ty = self.analyze_expression(expr.rhs.as_mut())?;

        match expr.op {
            PrefixOp::Neg => {
                if !(rhs_ty.is_numeric() || rhs_ty == Type::Any) {
                    return Err(CompileError::type_mismatch(
                        Type::Integer,
                        rhs_ty,
                        expr.rhs.span(),
                    ));
                }
            }
            PrefixOp::Not => {
                if !(rhs_ty.is_boolean() || rhs_ty == Type::Any) {
                    return Err(CompileError::type_mismatch(
                        Type::Boolean,
                        rhs_ty,
                        expr.rhs.span(),
                    ));
                }
            }
        }

        Ok(match expr.op {
            PrefixOp::Neg => rhs_ty,
            PrefixOp::Not => Type::Boolean,
        })
    }

    fn analyze_call_expression(&mut self, expr: &mut CallExpression) -> Result<Type, CompileError> {
        let func_ty = self.analyze_expression(expr.func.as_mut())?;

        if func_ty == Type::Any {
            return Ok(Type::Any);
        }

        if let Type::Decl(Declaration::Function(FunctionDeclaration {
            name,
            params,
            return_type,
        })) = &func_ty
        {
            if params.len() != expr.args.len() {
                return Err(CompileError::ArgumentCountMismatch {
                    expected: params.len(),
                    actual: expr.args.len(),
                });
            }

            for (arg, param_ty) in expr.args.iter_mut().zip(params.iter()) {
                let arg_ty = self.analyze_expression(arg)?;
                if let Some(ty) = &param_ty.1
                    && arg_ty != *ty
                    && arg_ty != Type::Any
                {
                    return Err(CompileError::type_mismatch(ty.clone(), arg_ty, arg.span()));
                };
            }

            Ok(return_type
                .as_ref()
                .map(|t| *t.clone())
                .unwrap_or(Type::Any))
        } else {
            Err(CompileError::NotCallable {
                ty: func_ty,
                span: expr.func.span(),
            })
        }
    }

    fn analyze_array_expression(
        &mut self,
        expr: &mut ArrayExpression,
    ) -> Result<Type, CompileError> {
        if expr.0.is_empty() {
            return Ok(Type::Array(Box::new(Type::Any)));
        }

        let mut elem_types = Vec::with_capacity(expr.0.len());

        // 为每个元素创建临时变量并分析类型
        for elem in expr.0.iter_mut() {
            let elem_ty = self.analyze_expression(elem)?;
            elem_types.push(elem_ty);
        }

        // 检查数组元素是否类型一致
        let first_ty = &elem_types[0];
        for (i, elem) in expr.0.iter().enumerate() {
            if &elem_types[i] != first_ty && elem_types[i] != Type::Any {
                return Err(CompileError::type_mismatch(
                    first_ty.clone(),
                    elem_types[i].clone(),
                    elem.span(),
                ));
            }
        }

        Ok(Type::Array(Box::new(first_ty.clone())))
    }

    fn analyze_map_expression(&mut self, _expr: &mut MapExpression) -> Result<Type, CompileError> {
        Ok(Type::Map(Box::new(Type::Any)))
    }

    fn analyze_index_get_expression(
        &mut self,
        expr: &mut IndexGetExpression,
    ) -> Result<Type, CompileError> {
        let object_ty = self.analyze_expression(expr.object.as_mut())?;
        let _index_ty = self.analyze_expression(expr.index.as_mut())?;

        // 根据集合类型确定返回类型
        match object_ty {
            Type::Array(ty) => Ok(*ty.clone()),
            Type::Map(value_ty) => Ok(*value_ty.clone()),
            Type::Any => Ok(Type::Any),
            _ => {
                // 其他不支持的集合类型
                Err(CompileError::InvalidOperation {
                    message: format!("Cannot index non-array and non-map type({object_ty:?})"),
                })
            }
        }
    }

    fn analyze_index_set_expression(
        &mut self,
        expr: &mut IndexSetExpression,
    ) -> Result<Type, CompileError> {
        let object_ty = self.analyze_expression(expr.object.as_mut())?;
        let index_ty = self.analyze_expression(expr.index.as_mut())?;
        let value_ty = self.analyze_expression(expr.value.as_mut())?;

        match object_ty {
            Type::Array(ty) => {
                if *ty != value_ty && value_ty != Type::Any {
                    return Err(CompileError::type_mismatch(
                        *ty,
                        value_ty,
                        expr.value.span(),
                    ));
                }
            }
            Type::Map(_) | Type::Any => {
                // 对于Map，Any对象，允许设置任何属性
            }
            _ => {
                // 其他不支持的集合类型
                return Err(CompileError::InvalidOperation {
                    message: "Cannot index non-array and non-map type".to_string(),
                });
            }
        }

        Ok(Type::Any)
    }

    fn analyze_property_get_expression(
        &mut self,
        expr: &mut PropertyGetExpression,
    ) -> Result<Type, CompileError> {
        let object_ty = self.analyze_expression(expr.object.as_mut())?;

        match object_ty {
            Type::Map(value_ty) if *value_ty == Type::String => Ok(*value_ty),
            Type::Any => {
                // 对于通用对象，允许访问任何属性
                Ok(Type::Any)
            }
            _ => {
                // 其他不支持的集合类型
                Err(CompileError::InvalidOperation {
                    message: "Cannot access property on non-object type".to_string(),
                })
            }
        }
    }

    fn analyze_property_set_expression(
        &mut self,
        expr: &mut PropertySetExpression,
    ) -> Result<Type, CompileError> {
        let object_ty = self.analyze_expression(expr.object.as_mut())?;
        let _property_ty = self.analyze_expression(expr.value.as_mut())?;

        match object_ty {
            Type::Map(value_ty) if *value_ty == Type::String => Ok(Type::Any),
            Type::Any => Ok(Type::Any), // 对于Any对象，允许设置任何属性
            _ => {
                // 其他不支持的集合类型
                Err(CompileError::InvalidOperation {
                    message: "Cannot access property on non-object type".to_string(),
                })
            }
        }
    }

    fn analyze_assign_expression(
        &mut self,
        expr: &mut AssignExpression,
    ) -> Result<Type, CompileError> {
        let object_ty = self.analyze_expression(expr.object.as_mut())?;
        let value_ty = self.analyze_expression(expr.value.as_mut())?;

        // 检查赋值左右类型是否兼容
        if object_ty != Type::Any && value_ty != Type::Any && object_ty != value_ty {
            return Err(CompileError::type_mismatch(
                object_ty,
                value_ty,
                expr.value.span(),
            ));
        }

        Ok(object_ty)
    }

    fn analyze_range_expression(
        &mut self,
        expr: &mut RangeExpression,
    ) -> Result<Type, CompileError> {
        if let Some(ref mut begin_expr) = expr.begin {
            let begin_ty = self.analyze_expression(begin_expr)?;
            if begin_ty != Type::Integer && begin_ty != Type::Any {
                return Err(CompileError::type_mismatch(
                    Type::Integer,
                    begin_ty,
                    begin_expr.span(),
                ));
            }
        }

        if let Some(ref mut end_expr) = expr.end {
            let end_ty = self.analyze_expression(end_expr)?;
            if end_ty != Type::Integer && end_ty != Type::Any {
                return Err(CompileError::type_mismatch(
                    Type::Integer,
                    end_ty,
                    end_expr.span(),
                ));
            }
        }

        Ok(Type::Range)
    }

    fn analyze_slice_expression(
        &mut self,
        expr: &mut SliceExpression,
    ) -> Result<Type, CompileError> {
        let object_ty = self.analyze_expression(expr.object.as_mut())?;
        self.analyze_range_expression(&mut expr.range.node)?;

        // 切片表达式的类型与原始对象类型相同
        Ok(object_ty)
    }

    fn analyze_try_expression(&mut self, expr: &mut ExpressionNode) -> Result<Type, CompileError> {
        let expr_ty = self.analyze_expression(expr)?;
        if expr_ty != Type::Any {
            return Err(CompileError::InvalidOperation {
                message: "Cannot try non-result type".to_string(),
            });
        }

        // Try表达式的类型与内部表达式类型相同
        Ok(expr_ty)
    }

    fn analyze_await_expression(
        &mut self,
        expr: &mut ExpressionNode,
    ) -> Result<Type, CompileError> {
        let expr_ty = self.analyze_expression(expr)?;
        if expr_ty != Type::Any {
            return Err(CompileError::InvalidOperation {
                message: "Cannot await non-promise type".to_string(),
            });
        }

        // Await表达式的类型与内部表达式类型相同
        Ok(expr_ty)
    }

    fn analyze_call_method_expression(
        &mut self,
        expr: &mut CallMethodExpression,
    ) -> Result<Type, CompileError> {
        let object_ty = self.analyze_expression(expr.object.as_mut())?;

        // 分析方法参数
        for arg in &mut expr.args {
            self.analyze_expression(arg)?;
        }

        // 方法调用的结果类型取决于方法实现，这里简单处理为Any类型
        Ok(Type::Any)
    }

    /// 从类型表达式转换为Type
    fn type_from_type_expr(&mut self, type_expr: &TypeExpression) -> Result<Type, CompileError> {
        let ty = self.type_cx.resolve_type_decl(type_expr);
        if ty == Type::Unknown {
            Err(CompileError::UnknownType {
                name: format!("{type_expr:?}"),
            })
        } else {
            Ok(ty)
        }
    }
}
