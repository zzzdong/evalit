use super::CompileError;
use super::ast::syntax::*;
use super::symbol::SymbolTable;
use super::typing::TypeContext;
use crate::Environment;

pub struct SemanticAnalyzer<'a> {
    type_cx: &'a TypeContext,
    loop_depth: usize,
    symbol_table: SymbolTable<()>,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(type_cx: &'a TypeContext) -> Self {
        SemanticAnalyzer {
            type_cx,
            loop_depth: 0,
            symbol_table: SymbolTable::new(),
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
            self.symbol_table.insert(name.clone(), ());
        }

        // 第二阶段：分析所有语句
        for stmt in &mut program.stmts {
            self.analyze_statement(stmt)?;
        }
        Ok(())
    }

    /// 分析语句并推断类型
    fn analyze_statement(&mut self, stmt: &StatementNode) -> Result<(), CompileError> {
        let span = stmt.span;

        match &stmt.node {
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
            Statement::Item(ItemStatement::Function(func)) => self.analyze_function_item(func),
            Statement::Item(ItemStatement::Struct(item)) => self.analyze_struct_item(item),
            Statement::Item(ItemStatement::Enum(EnumItem { .. })) => {
                unimplemented!("EnumItem not implemented")
            }
        }
    }

    fn analyze_let_statement(&mut self, let_stmt: &LetStatement) -> Result<(), CompileError> {
        let LetStatement { name, ty, value } = let_stmt;

        self.symbol_table.insert(name, ());

        Ok(())
    }

    /// 分析If语句
    fn analyze_if_statement(&mut self, if_stmt: &IfStatement) -> Result<(), CompileError> {
        // 分析条件表达式
        self.analyze_expression(&if_stmt.condition)?;

        // 分析then分支
        self.analyze_block(&if_stmt.then_branch)?;

        // 分析else分支（如果有）
        if let Some(else_branch) = &if_stmt.else_branch {
            self.analyze_block(else_branch)?;
        }

        Ok(())
    }

    fn analyze_loop_statement(&mut self, loop_stmt: &LoopStatement) -> Result<(), CompileError> {
        // 增加循环深度
        self.loop_depth += 1;

        // 分析循环体
        self.analyze_block(&loop_stmt.body)?;

        // 减少循环深度
        self.loop_depth -= 1;

        Ok(())
    }

    /// 分析While语句
    fn analyze_while_statement(&mut self, while_stmt: &WhileStatement) -> Result<(), CompileError> {
        // 分析条件表达式
        self.analyze_expression(&while_stmt.condition)?;

        // 增加循环深度
        self.loop_depth += 1;

        // 分析循环体
        self.analyze_block(&while_stmt.body)?;

        // 减少循环深度
        self.loop_depth -= 1;

        Ok(())
    }

    /// 分析For语句
    fn analyze_for_statement(&mut self, for_stmt: &ForStatement) -> Result<(), CompileError> {
        // 创建新的作用域
        self.symbol_table.enter_scope();

        // 分析迭代器表达式
        // self.analyze_expression(&mut for_stmt.iterable)?;

        self.analyze_pattern(&for_stmt.pat, &for_stmt.iterable)?;

        // 增加循环深度
        self.loop_depth += 1;

        // 分析循环体
        self.analyze_block(&for_stmt.body)?;

        // 减少循环深度
        self.loop_depth -= 1;

        // 恢复作用域
        self.symbol_table.leave_scope();

        Ok(())
    }

    fn analyze_pattern(
        &mut self,
        pattern: &Pattern,
        expr: &ExpressionNode,
    ) -> Result<(), CompileError> {
        self.analyze_expression(expr)?;

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
        return_stmt: &ReturnStatement,
        _span: Span,
    ) -> Result<(), CompileError> {
        if let Some(expr) = &return_stmt.value {
            self.analyze_expression(expr)?;
        }
        Ok(())
    }

    /// 分析代码块
    fn analyze_block(&mut self, block: &BlockStatement) -> Result<(), CompileError> {
        // 创建新的作用域
        self.symbol_table.enter_scope();

        // 分析块中的所有语句
        for stmt in &block.0 {
            self.analyze_statement(stmt)?;
        }

        // 恢复作用域
        self.symbol_table.leave_scope();

        Ok(())
    }

    /// 分析函数定义
    fn analyze_function_item(&mut self, func: &FunctionItem) -> Result<(), CompileError> {
        // 创建新的作用域
        self.symbol_table.enter_scope();

        // 添加参数到环境
        for param in &func.params {
            self.symbol_table.insert(&param.name, ());
        }

        // 分析函数体
        self.analyze_block(&func.body)?;

        // 恢复环境
        self.symbol_table.leave_scope();

        Ok(())
    }

    fn analyze_struct_item(&mut self, _item: &StructItem) -> Result<(), CompileError> {
        Ok(())
    }

    /// 分析表达式并推断类型
    fn analyze_expression(&mut self, expr: &ExpressionNode) -> Result<(), CompileError> {
        match &expr.node {
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
            }
        };

        Ok(())
    }

    fn anlyze_identifier_expression(
        &mut self,
        ident: &IdentifierExpression,
    ) -> Result<(), CompileError> {
        if !self.type_cx.type_is_defined(&ident.0) {
            return Err(CompileError::UndefinedVariable {
                name: ident.0.clone(),
            });
        }

        Ok(())
    }

    fn analyze_binary_expression(&mut self, expr: &BinaryExpression) -> Result<(), CompileError> {
        let lhs_ty = self.analyze_expression(&expr.lhs)?;
        let rhs_ty = self.analyze_expression(&expr.rhs)?;

        Ok(())
    }

    fn analyze_prefix_expression(&mut self, expr: &PrefixExpression) -> Result<(), CompileError> {
        let rhs_ty = self.analyze_expression(&expr.rhs)?;

        Ok(())
    }

    fn analyze_call_expression(&mut self, expr: &CallExpression) -> Result<(), CompileError> {
        self.analyze_expression(&expr.func)?;

        for arg in expr.args.iter() {
            self.analyze_expression(&arg)?;
        }

        Ok(())
    }

    fn analyze_array_expression(&mut self, expr: &ArrayExpression) -> Result<(), CompileError> {
        // 为每个元素创建临时变量并分析类型
        for elem in expr.0.iter() {
            self.analyze_expression(elem)?;
        }

        Ok(())
    }

    fn analyze_map_expression(&mut self, expr: &MapExpression) -> Result<(), CompileError> {
        for (key, value) in expr.0.iter() {
            self.analyze_expression(key)?;
            self.analyze_expression(value)?;
        }

        Ok(())
    }

    fn analyze_index_get_expression(
        &mut self,
        expr: &IndexGetExpression,
    ) -> Result<(), CompileError> {
        self.analyze_expression(&expr.object)?;
        self.analyze_expression(&expr.index)?;

        Ok(())
    }

    fn analyze_index_set_expression(
        &mut self,
        expr: &IndexSetExpression,
    ) -> Result<(), CompileError> {
        self.analyze_expression(&expr.object)?;
        self.analyze_expression(&expr.value)?;

        Ok(())
    }

    fn analyze_property_get_expression(
        &mut self,
        expr: &PropertyGetExpression,
    ) -> Result<(), CompileError> {
        self.analyze_expression(&expr.object)?;
        Ok(())
    }

    fn analyze_property_set_expression(
        &mut self,
        expr: &PropertySetExpression,
    ) -> Result<(), CompileError> {
        self.analyze_expression(&expr.object)?;
        self.analyze_expression(&expr.value)?;

        Ok(())
    }

    fn analyze_assign_expression(&mut self, expr: &AssignExpression) -> Result<(), CompileError> {
        self.analyze_expression(&expr.object)?;
        self.analyze_expression(&expr.value)?;

        Ok(())
    }

    fn analyze_range_expression(&mut self, expr: &RangeExpression) -> Result<(), CompileError> {
        if let Some(ref begin_expr) = expr.begin {
            self.analyze_expression(begin_expr)?;
        }

        if let Some(ref end_expr) = expr.end {
            self.analyze_expression(end_expr)?
        }

        Ok(())
    }

    fn analyze_slice_expression(&mut self, expr: &SliceExpression) -> Result<(), CompileError> {
        self.analyze_expression(&expr.object)?;
        self.analyze_range_expression(&expr.range.node)?;

        Ok(())
    }

    fn analyze_try_expression(&mut self, expr: &ExpressionNode) -> Result<(), CompileError> {
        self.analyze_expression(expr)?;

        Ok(())
    }

    fn analyze_await_expression(&mut self, expr: &ExpressionNode) -> Result<(), CompileError> {
        self.analyze_expression(expr)?;

        Ok(())
    }

    fn analyze_call_method_expression(
        &mut self,
        expr: &CallMethodExpression,
    ) -> Result<(), CompileError> {
        self.analyze_expression(&expr.object)?;

        // 分析方法参数
        for arg in &expr.args {
            self.analyze_expression(arg)?;
        }

        Ok(())
    }
}
