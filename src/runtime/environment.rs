use std::collections::BTreeMap;

use super::{Callable, NativeFunction, Object, ValueRef};

#[derive(Debug, Clone)]
pub enum EnvVariable {
    Value(ValueRef),
    Function(ValueRef),
}

#[derive(Debug, Clone)]
pub struct Environment {
    pub(crate) symbols: BTreeMap<String, EnvVariable>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            symbols: BTreeMap::new(),
        }
    }

    pub fn with_variable<T: Object>(mut self, name: impl ToString, value: T) -> Self {
        self.insert(name, value);
        self
    }

    pub fn with_function<Args: 'static>(
        mut self,
        name: impl ToString,
        callable: impl Callable<Args> + Send + Sync + 'static,
    ) -> Self {
        self.define_function(name, callable);
        self
    }

    pub fn insert<T: Object>(&mut self, name: impl ToString, value: T) {
        self.symbols
            .insert(name.to_string(), EnvVariable::Value(ValueRef::new(value)));
    }

    pub fn define_function<Args: 'static>(
        &mut self,
        name: impl ToString,
        callable: impl Callable<Args> + Send + Sync + 'static,
    ) {
        self.symbols.insert(
            name.to_string(),
            EnvVariable::Function(ValueRef::new(NativeFunction::new(
                name,
                Box::new(callable.into_function()),
            ))),
        );
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&EnvVariable> {
        self.symbols.get(name.as_ref())
    }

    pub fn remove_as<T: Object>(&mut self, name: impl AsRef<str>) -> Option<T> {
        match self.symbols.remove(name.as_ref()) {
            Some(EnvVariable::Value(value)) => {
                let object = value.take();
                object.into_inner().ok()
            }
            _ => None,
        }
    }

    // pub fn get_var(&self, name: impl AsRef<str>) -> Option<&&'a mut dyn Object> {
    //     self.symbols.get(name.as_ref()).and_then(|val| match val {
    //         EnvVariable::Value(value) => Some(value),
    //         _ => None,
    //     })
    // }

    // pub fn get_function_mut(&self, name: impl AsRef<str>) -> Option<&NativeFunction> {
    //     self.symbols.get(name.as_ref()).and_then(|val| match val {
    //         EnvVariable::Function(func) => Some(func),
    //         _ => None,
    //     })
    // }
}
