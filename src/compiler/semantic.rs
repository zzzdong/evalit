use super::CompileError;
use crate::{Environment, ast::*};
use std::collections::HashMap;

pub struct SemanticAnalyzer {
    // 类型环境，存储变量名到类型的映射
    type_env: HashMap<String, Type>,
    // 函数环境，存储函数名到签名的映射
    fn_env: HashMap<String, Type>,
    // 当前函数的返回类型，用于检查return语句
    current_function_return_type: Option<Type>,
    // 循环嵌套计数，用于检查break和continue语句
    loop_depth: usize,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            type_env: HashMap::new(),
            fn_env: HashMap::new(),
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
            self.type_env.insert(name.to_string(), Type::EnvObject);
        }

        // 第二阶段：收集函数声明
        for stmt in &program.stmts {
            if let Statement::Item(ItemStatement::Fn(FunctionItem {
                name,
                params,
                return_ty,
                ..
            })) = &stmt.node
            {
                let param_types = params
                    .iter()
                    .map(|p| {
                        p.ty.clone()
                            .map(|t| self.type_from_type_expr(&t))
                            .transpose()
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let return_ty = return_ty
                    .as_ref()
                    .map(|t| self.type_from_type_expr(t))
                    .transpose()?;

                self.fn_env.insert(
                    name.clone(),
                    Type::Function {
                        params: param_types,
                        return_ty: return_ty.map(Box::new),
                    },
                );
            }
        }

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
            Statement::Let(LetStatement { name, ty, value }) => {
                if let Some(expr) = value {
                    self.analyze_expression(expr)?;

                    if let Some(type_expr) = ty {
                        let expected_type = self.type_from_type_expr(type_expr)?;
                        if expected_type != Type::Unknown && expr.ty != expected_type {
                            return Err(CompileError::TypeMismatch {
                                expected: expected_type,
                                actual: expr.ty.clone(),
                                span: expr.span,
                            });
                        }
                    }

                    self.type_env.insert(name.clone(), expr.ty.clone());
                }
                Ok(())
            }
            Statement::Expression(expr) => self.analyze_expression(expr),
            Statement::Item(ItemStatement::Fn(func)) => self.analyze_function(func),
            Statement::If(if_stmt) => self.analyze_if_statement(if_stmt),
            Statement::Loop(loop_stmt) => self.analyze_loop_statement(loop_stmt),
            Statement::While(while_stmt) => self.analyze_while_statement(while_stmt),
            Statement::For(for_stmt) => self.analyze_for_statement(for_stmt),
            Statement::Return(return_stmt) => self.analyze_return_statement(return_stmt, span),
            Statement::Block(block) => self.analyze_block(block),
            Statement::Break => self.analyze_break_statement(span),
            Statement::Continue => self.analyze_continue_statement(span),
            Statement::Empty => Ok(()),
            Statement::Item(ItemStatement::Struct(StructItem { .. })) => {
                unimplemented!("StructItem not implemented")
            }
            Statement::Item(ItemStatement::Enum(EnumItem { .. })) => {
                unimplemented!("EnumItem not implemented")
            }
        }
    }

    /// 分析If语句
    fn analyze_if_statement(&mut self, if_stmt: &mut IfStatement) -> Result<(), CompileError> {
        // 分析条件表达式
        self.analyze_expression(&mut if_stmt.condition)?;

        // TODO: 条件必须是布尔类型
        // if if_stmt.condition.ty != Type::Boolean {
        //     return Err(CompileError::TypeMismatch {
        //         expected: Type::Boolean,
        //         actual: if_stmt.condition.ty.clone(),
        //         span: if_stmt.condition.span,
        //     });
        // }

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
        self.analyze_expression(&mut while_stmt.condition)?;

        // 条件必须是布尔类型
        if !while_stmt.condition.ty.is_boolean() {
            return Err(CompileError::TypeMismatch {
                expected: Type::Boolean,
                actual: while_stmt.condition.ty.clone(),
                span: while_stmt.condition.span,
            });
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
        let old_env = self.type_env.clone();

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
        self.type_env = old_env;

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
                // self.type_env.insert(id.clone(), expr.ty.clone());
                // TODO
                self.type_env.insert(id.clone(), Type::Void);
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

        // TODO: 检查函数返回类型是否匹配
        // 获取当前函数的返回类型
        // let expected_return_type = match &self.current_function_return_type {
        //     Some(ty) => ty.clone(),
        //     None => {
        //         return Err(CompileError::ReturnOutsideFunction {
        //             span: span,
        //         });
        //     }
        // };

        // match (return_stmt.value, &expected_return_type) {
        //     // 有返回值，且函数返回类型不是void
        //     (Some(expr), ty) if *ty != Type::Void => {
        //         self.analyze_expression(&mut expr)?;

        //         // 检查返回值类型是否匹配
        //         if expr.ty != *ty && *ty != Type::Unknown {
        //             return Err(CompileError::TypeMismatch {
        //                 expected: ty.clone(),
        //                 actual: expr.ty.clone(),
        //                 span: expr.span,
        //             });
        //         }
        //     }
        //     // 无返回值，但函数返回类型不是void
        //     (None, ty) if *ty != Type::Void && *ty != Type::Unknown => {
        //         return Err(CompileError::MissingReturnValue {
        //             expected: ty.clone(),
        //             span: 0, // 这里需要一个合适的span
        //         });
        //     }
        //     // 有返回值，但函数返回类型是void
        //     (Some(expr), Type::Void) => {
        //         self.analyze_expression(expr)?;
        //         return Err(CompileError::UnexpectedReturnValue { span: expr.span });
        //     }
        //     // 其他情况都是合法的
        //     _ => {}
        // }

        // Ok(())
    }

    /// 分析代码块
    fn analyze_block(&mut self, block: &mut BlockStatement) -> Result<(), CompileError> {
        // 创建新的作用域
        let old_env = self.type_env.clone();

        // 分析块中的所有语句
        for stmt in &mut block.0 {
            self.analyze_statement(stmt)?;
        }

        // 恢复作用域
        self.type_env = old_env;

        Ok(())
    }

    /// 分析函数定义
    fn analyze_function(&mut self, func: &mut FunctionItem) -> Result<(), CompileError> {
        // 创建新的作用域
        let mut local_env = self.type_env.clone();

        // 获取函数返回类型
        let return_ty = if let Some(ty_expr) = &func.return_ty {
            self.type_from_type_expr(ty_expr)?
        } else {
            Type::Unknown
        };

        // 添加参数到环境
        for param in &func.params {
            let param_ty = match &param.ty {
                Some(ty) => self.type_from_type_expr(ty)?,
                None => Type::Void,
            };

            local_env.insert(param.name.clone(), param_ty);
        }

        // 保存当前函数的返回类型，用于检查return语句
        let old_return_type = self.current_function_return_type.replace(return_ty.clone());

        // 分析函数体
        let old_env = std::mem::replace(&mut self.type_env, local_env);
        for stmt in &mut func.body.0 {
            self.analyze_statement(stmt)?;
        }

        // 恢复环境
        self.type_env = old_env;
        self.current_function_return_type = old_return_type;

        Ok(())
    }

    /// 分析表达式并推断类型
    fn analyze_expression(&mut self, expr: &mut ExpressionNode) -> Result<(), CompileError> {
        match &mut expr.node {
            Expression::Literal(lit) => {
                expr.ty = match lit {
                    LiteralExpression::Null => Type::Unknown,
                    LiteralExpression::Boolean(_) => Type::Boolean,
                    LiteralExpression::Integer(_) => Type::Integer,
                    LiteralExpression::Float(_) => Type::Float,
                    LiteralExpression::Char(_) => Type::Char,
                    LiteralExpression::String(_) => Type::String,
                };
                Ok(())
            }
            Expression::Identifier(ident) => {
                // 先从变量环境查找
                if let Some(ty) = self.type_env.get(&ident.0) {
                    expr.ty = ty.clone();
                    return Ok(());
                }
                // 再从函数环境查找
                if let Some(ty) = self.fn_env.get(&ident.0) {
                    expr.ty = ty.clone();
                    return Ok(());
                }
                Err(CompileError::UndefinedVariable {
                    name: ident.0.clone(),
                    span: expr.span,
                })
            }
            Expression::Binary(BinaryExpression { op, lhs, rhs }) => {
                self.analyze_expression(lhs)?;
                self.analyze_expression(rhs)?;

                if lhs.ty == Type::Void
                    || rhs.ty == Type::Void
                    || lhs.ty == Type::Unknown
                    || rhs.ty == Type::Unknown
                {
                    expr.ty = Type::Void;
                    return Ok(()); // void类型可以和任何类型比较
                }

                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                        if (!lhs.ty.is_numeric() || rhs.ty != lhs.ty)
                            && lhs.ty != Type::String
                            && lhs.ty != Type::Char
                        {
                            return Err(CompileError::TypeMismatch {
                                expected: Type::Integer,
                                actual: lhs.ty.clone(),
                                span: lhs.span,
                            });
                        }
                    }
                    BinOp::LogicAnd | BinOp::LogicOr => {
                        if lhs.ty != Type::Boolean || rhs.ty != Type::Boolean {
                            return Err(CompileError::TypeMismatch {
                                expected: Type::Boolean,
                                actual: lhs.ty.clone(),
                                span: lhs.span,
                            });
                        }
                    }
                    _ => {}
                }

                expr.ty = match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                        lhs.ty.clone()
                    }
                    BinOp::LogicAnd | BinOp::LogicOr => Type::Boolean,
                    BinOp::Less
                    | BinOp::LessEqual
                    | BinOp::Greater
                    | BinOp::GreaterEqual
                    | BinOp::Equal
                    | BinOp::NotEqual => Type::Boolean,
                    _ => Type::Unknown,
                };
                Ok(())
            }
            Expression::Prefix(PrefixExpression { op, rhs }) => {
                self.analyze_expression(rhs)?;

                match op {
                    PrefixOp::Neg => {
                        if !rhs.ty.is_numeric() {
                            return Err(CompileError::TypeMismatch {
                                expected: Type::Integer,
                                actual: rhs.ty.clone(),
                                span: rhs.span,
                            });
                        }
                    }
                    PrefixOp::Not => {
                        if !rhs.ty.is_boolean() {
                            return Err(CompileError::TypeMismatch {
                                expected: Type::Boolean,
                                actual: rhs.ty.clone(),
                                span: rhs.span,
                            });
                        }
                    }
                }

                expr.ty = match op {
                    PrefixOp::Neg => rhs.ty.clone(),
                    PrefixOp::Not => Type::Boolean,
                };
                Ok(())
            }
            Expression::Call(CallExpression { func, args }) => {
                self.analyze_expression(func)?;

                if let Type::Function { params, return_ty } = &func.ty {
                    if params.len() != args.len() {
                        return Err(CompileError::ArgumentCountMismatch {
                            expected: params.len(),
                            actual: args.len(),
                            span: expr.span,
                        });
                    }

                    for (arg, param_ty) in args.iter_mut().zip(params.iter()) {
                        self.analyze_expression(arg)?;
                        if let Some(ty) = param_ty {
                            if &arg.ty != ty && arg.ty != Type::Void && arg.ty != Type::Unknown {
                                return Err(CompileError::TypeMismatch {
                                    expected: ty.clone(),
                                    actual: arg.ty.clone(),
                                    span: arg.span,
                                });
                            };
                        }
                    }

                    let return_ty = return_ty
                        .as_ref()
                        .map(|ty| *(ty.clone()))
                        .unwrap_or(Type::Unknown);

                    expr.ty = return_ty;
                    Ok(())
                } else if func.ty == Type::EnvObject {
                    // when func is a dyn object from env
                    return Ok(());
                } else {
                    // Err(CompileError::NotCallable {
                    //     ty: func.ty.clone(),
                    //     span: func.span,
                    // })
                    // TODO: property call
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }

    /// 从类型表达式转换为Type
    fn type_from_type_expr(&self, type_expr: &TypeExpression) -> Result<Type, CompileError> {
        match type_expr {
            TypeExpression::Boolean => Ok(Type::Boolean),
            TypeExpression::Byte => Ok(Type::Byte),
            TypeExpression::Integer => Ok(Type::Integer),
            TypeExpression::Float => Ok(Type::Float),
            TypeExpression::Char => Ok(Type::Char),
            TypeExpression::String => Ok(Type::String),
            TypeExpression::UserDefined(_) => unimplemented!("UserDefined"),
            _ => Ok(Type::Unknown),
        }
    }
}
