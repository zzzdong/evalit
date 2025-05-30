use super::syntax::*;

pub trait Walker {
    type Error;
    type StatementResult: Default;
    type ExpressionResult: Default;

    fn walk_program(&mut self, program: &Program) -> Result<(), Self::Error> {
        for stmt in &program.stmts {
            self.walk_statement(stmt)?;
        }
        Ok(())
    }

    fn walk_statement(
        &mut self,
        stmt: &StatementNode,
    ) -> Result<Self::StatementResult, Self::Error> {
        match &stmt.node {
            Statement::Empty => self.walk_empty(),
            Statement::Break => self.walk_break(),
            Statement::Continue => self.walk_continue(),
            Statement::Block(stmt) => self.walk_block_statement(stmt),
            Statement::Item(stmt) => self.walk_item_statement(stmt),
            Statement::Let(stmt) => self.walk_let_statement(stmt),
            Statement::For(stmt) => self.walk_for_statement(stmt),
            Statement::While(stmt) => self.walk_while_statement(stmt),
            Statement::Loop(stmt) => self.walk_loop_statement(stmt),
            Statement::If(stmt) => self.walk_if_statement(stmt),
            Statement::Return(stmt) => self.walk_return_statement(stmt),
            Statement::Expression(expr) => self.walk_expression(expr).map(|_| Default::default()),
        }
    }

    fn walk_expression(
        &mut self,
        expr: &ExpressionNode,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        match &expr.node {
            Expression::Literal(expr) => self.walk_literal_expression(expr),
            Expression::Identifier(expr) => self.walk_identifier_expression(expr),
            Expression::Environment(expr) => self.walk_environment_expression(expr),
            Expression::Path(expr) => self.walk_path_expression(expr),
            Expression::Tuple(expr) => self.walk_tuple_expression(expr),
            Expression::Array(expr) => self.walk_array_expression(expr),
            Expression::Map(expr) => self.walk_map_expression(expr),
            Expression::Closure(expr) => self.walk_closure_expression(expr),
            Expression::Range(expr) => self.walk_range_expression(expr),
            Expression::Slice(expr) => self.walk_slice_expression(expr),
            Expression::Assign(expr) => self.walk_assign_expression(expr),
            Expression::Call(expr) => self.walk_call_expression(expr),
            Expression::Try(expr) => self.walk_expression(expr),
            Expression::Await(expr) => self.walk_expression(expr),
            Expression::Prefix(expr) => self.walk_prefix_expression(expr),
            Expression::Binary(expr) => self.walk_binary_expression(expr),
            Expression::IndexGet(expr) => self.walk_index_get_expression(expr),
            Expression::IndexSet(expr) => self.walk_index_set_expression(expr),
            Expression::PropertyGet(expr) => self.walk_property_get_expression(expr),
            Expression::PropertySet(expr) => self.walk_property_set_expression(expr),
            Expression::CallMethod(expr) => self.walk_call_method_expression(expr),
            Expression::StructExpr(expr) => self.walk_struct_expression(expr),
        }
    }

    fn walk_empty(&mut self) -> Result<Self::StatementResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_break(&mut self) -> Result<Self::StatementResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_continue(&mut self) -> Result<Self::StatementResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_block_statement(
        &mut self,
        stmt: &BlockStatement,
    ) -> Result<Self::StatementResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_item_statement(
        &mut self,
        stmt: &ItemStatement,
    ) -> Result<Self::StatementResult, Self::Error> {
        match stmt {
            ItemStatement::Function(item) => {
                self.walk_function_item(item)?;
                Ok(Default::default())
            }
            ItemStatement::Struct(item) => {
                self.walk_struct_item(item)?;
                Ok(Default::default())
            }
            ItemStatement::Enum(item) => {
                self.walk_enum_item(item)?;
                Ok(Default::default())
            }
        }
    }

    fn walk_function_item(&mut self, item: &FunctionItem) -> Result<(), Self::Error> {
        Ok(Default::default())
    }

    fn walk_struct_item(&mut self, item: &StructItem) -> Result<(), Self::Error> {
        Ok(Default::default())
    }

    fn walk_enum_item(&mut self, item: &EnumItem) -> Result<(), Self::Error> {
        Ok(Default::default())
    }

    fn walk_let_statement(
        &mut self,
        stmt: &LetStatement,
    ) -> Result<Self::StatementResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_for_statement(
        &mut self,
        stmt: &ForStatement,
    ) -> Result<Self::StatementResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_while_statement(
        &mut self,
        stmt: &WhileStatement,
    ) -> Result<Self::StatementResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_loop_statement(
        &mut self,
        stmt: &LoopStatement,
    ) -> Result<Self::StatementResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_if_statement(
        &mut self,
        stmt: &IfStatement,
    ) -> Result<Self::StatementResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_return_statement(
        &mut self,
        stmt: &ReturnStatement,
    ) -> Result<Self::StatementResult, Self::Error> {
        Ok(Default::default())
    }

    // 表达式遍历方法
    fn walk_literal_expression(
        &mut self,
        _expr: &LiteralExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_identifier_expression(
        &mut self,
        _expr: &IdentifierExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_environment_expression(
        &mut self,
        _expr: &EnvironmentExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_path_expression(
        &mut self,
        _expr: &PathExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_tuple_expression(
        &mut self,
        _expr: &TupleExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_array_expression(
        &mut self,
        _expr: &ArrayExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_map_expression(
        &mut self,
        _expr: &MapExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_closure_expression(
        &mut self,
        _expr: &ClosureExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_range_expression(
        &mut self,
        _expr: &RangeExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_slice_expression(
        &mut self,
        _expr: &SliceExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_assign_expression(
        &mut self,
        _expr: &AssignExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_call_expression(
        &mut self,
        _expr: &CallExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_prefix_expression(
        &mut self,
        _expr: &PrefixExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_binary_expression(
        &mut self,
        _expr: &BinaryExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_index_get_expression(
        &mut self,
        _expr: &IndexGetExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_index_set_expression(
        &mut self,
        _expr: &IndexSetExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_property_get_expression(
        &mut self,
        _expr: &PropertyGetExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_property_set_expression(
        &mut self,
        _expr: &PropertySetExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_call_method_expression(
        &mut self,
        _expr: &CallMethodExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }

    fn walk_struct_expression(
        &mut self,
        _expr: &StructExpression,
    ) -> Result<Self::ExpressionResult, Self::Error> {
        Ok(Default::default())
    }
}
