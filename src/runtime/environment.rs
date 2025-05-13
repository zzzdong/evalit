use std::collections::HashMap;

use super::{Callable, NativeFunction, ValueRef};

#[derive(Debug, Clone)]
pub struct Environment {
    pub(crate) symbols: HashMap<String, ValueRef>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            symbols: HashMap::new(),
        }
    }

    pub fn with_variable(mut self, name: impl ToString, value: impl Into<ValueRef>) -> Self {
        self.define(name, value);
        self
    }

    pub fn with_function<Args: 'static>(
        mut self,
        name: impl ToString,
        callable: impl Callable<Args> + Send,
    ) -> Self {
        self.define_function(name, callable);
        self
    }

    pub fn define(&mut self, name: impl ToString, value: impl Into<ValueRef>) {
        self.symbols.insert(name.to_string(), value.into());
    }

    // pub fn define_function<F>(&mut self, name: impl ToString, func: F)
    // where
    //     F: Fn(&[ValueRef]) -> Result<Option<Value>, RuntimeError> + 'static,
    // {
    //     let Value = Value.to_string();
    //     self.define(Value.clone(), NativeFunction::new(Value, Box::new(func)));
    // }

    pub fn define_function<Args: 'static>(
        &mut self,
        name: impl ToString,
        callable: impl Callable<Args> + Send,
    ) {
        self.define(
            name.to_string(),
            ValueRef::new(NativeFunction::new(
                name,
                Box::new(callable.into_function()),
            )),
        );
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<ValueRef> {
        self.symbols.get(name.as_ref()).cloned()
    }
}
