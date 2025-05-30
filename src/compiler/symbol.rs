use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTable<T> {
    scopes: Vec<Scope<T>>,
}

impl<T> SymbolTable<T> {
    pub fn new() -> Self {
        SymbolTable::<T> {
            scopes: vec![Scope::<T>::new()],
        }
    }

    pub fn lookup(&self, name: impl AsRef<str>) -> Option<&T> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.variables.get(name.as_ref()) {
                return Some(ty);
            }
        }
        None
    }

    pub fn insert(&mut self, name: impl Into<String>, value: T) {
        self.scopes
            .last_mut()
            .unwrap()
            .variables
            .insert(name.into(), value);
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::<T>::new());
    }

    pub fn leave_scope(&mut self) {
        self.scopes.pop();
    }
}

#[derive(Debug)]
struct Scope<T> {
    variables: HashMap<String, T>,
}

impl<T> Scope<T> {
    fn new() -> Self {
        Scope {
            variables: HashMap::new(),
        }
    }
}
