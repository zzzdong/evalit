use std::collections::HashMap;

use crate::Environment;

use super::ast::syntax::*;

#[derive(Debug, Clone)]
pub struct TypeContext {
    type_decls: HashMap<String, Declaration>,
    type_env: HashMap<String, Type>,
    // 新增：用于缓存已解析的类型声明
    resolved_types: HashMap<String, Type>,
}

impl TypeContext {
    pub fn new() -> Self {
        TypeContext {
            type_decls: HashMap::new(),
            type_env: HashMap::new(),
            resolved_types: HashMap::new(), // 初始化缓存
        }
    }

    pub fn add_type_decl(&mut self, name: String, decl: Declaration) {
        self.type_decls.insert(name.clone(), decl.clone());

        if matches!(&decl, &Declaration::Function(_)) {
            self.type_env.insert(name.to_string(), Type::Decl(decl));
        }
    }

    pub fn get_type_decl(&self, name: &str) -> Option<&Declaration> {
        self.type_decls.get(name)
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.type_env.get(name)
    }

    pub fn set_type(&mut self, name: String, ty: Type) {
        self.type_env.insert(name, ty);
    }

    pub fn function_decls(&self) -> impl Iterator<Item = &Declaration> {
        self.type_decls.values().filter(|decl| decl.is_function())
    }

    pub fn process_env(&mut self, env: &Environment) {
        for (name, _value) in env.symbols.iter() {
            self.set_type(name.clone(), Type::Any);
        }
    }

    pub fn analyze_type_decl(&mut self, stmts: &[StatementNode]) {
        for stmt in stmts {
            match &stmt.node {
                Statement::Item(ItemStatement::Fn(func)) => {
                    let FunctionItem {
                        name,
                        params,
                        return_ty,
                        body,
                    } = func;

                    let mut param_types = Vec::new();

                    for param in params {
                        let ty = param.ty.clone().map(|t| self.resolve_type_decl(&t));

                        param_types.push((param.name.clone(), ty));
                    }

                    let return_ty = return_ty
                        .as_ref()
                        .map(|t| Box::new(self.resolve_type_decl(t)));

                    let func_decl = FunctionDeclaration {
                        name: name.clone(),
                        params: param_types,
                        return_type: return_ty,
                    };

                    self.add_type_decl(name.clone(), Declaration::Function(func_decl));
                }
                Statement::Item(ItemStatement::Struct(item)) => {
                    let StructItem { name, fields } = item;
                    let struct_decl = StructDeclaration {
                        name: name.clone(),
                        fields: fields
                            .iter()
                            .map(|field| (field.name.clone(), self.resolve_type_decl(&field.ty)))
                            .collect(),
                    };

                    self.add_type_decl(name.clone(), Declaration::Struct(struct_decl));
                }
                Statement::Item(ItemStatement::Enum(EnumItem { .. })) => {}
                _ => {}
            }
        }
    }

    // 新增：递归解析类型声明
    fn resolve_type_decl_recursive(&mut self, type_expr: &TypeExpression) -> Type {
        match type_expr {
            TypeExpression::Any => Type::Any,
            TypeExpression::Boolean => Type::Boolean,
            TypeExpression::Byte => Type::Byte,
            TypeExpression::Integer => Type::Integer,
            TypeExpression::Float => Type::Float,
            TypeExpression::Char => Type::Char,
            TypeExpression::String => Type::String,
            TypeExpression::Tuple(types) => {
                let types = types.iter().map(|ty| self.resolve_type_decl(ty)).collect();
                Type::Tuple(types)
            }
            TypeExpression::Array(ty) => {
                let ty = self.resolve_type_decl(ty);
                Type::Array(Box::new(ty))
            }
            TypeExpression::UserDefined(ty) => {
                // 检查缓存中是否存在已解析的类型
                if let Some(cached_type) = self.resolved_types.get(ty) {
                    return cached_type.clone();
                }

                // 检查是否已经存在于 type_env 中
                if let Some(ty) = self.get_type(ty) {
                    return ty.clone();
                }

                // 解析类型声明
                if let Some(decl) = self.get_type_decl(ty) {
                    let resolved_type = match decl {
                        Declaration::Function(func_decl) => {
                            Type::Decl(Declaration::Function(func_decl.clone()))
                        }
                        Declaration::Struct(struct_decl) => {
                            Type::Decl(Declaration::Struct(struct_decl.clone()))
                        }
                        Declaration::Enum(enum_decl) => {
                            Type::Decl(Declaration::Enum(enum_decl.clone()))
                        }
                    };

                    // 更新缓存
                    self.resolved_types
                        .insert(ty.clone(), resolved_type.clone());
                    self.set_type(ty.clone(), resolved_type.clone());
                    resolved_type
                } else {
                    // 如果无法解析，则返回 UserDefined
                    Type::UserDefined(ty.clone())
                }
            }
            _ => Type::Any,
        }
    }

    // 新增：对外暴露的解析函数
    pub fn resolve_type_decl(&mut self, type_expr: &TypeExpression) -> Type {
        self.resolve_type_decl_recursive(type_expr)
    }
}
