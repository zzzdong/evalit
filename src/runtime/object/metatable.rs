use std::{collections::HashMap, fmt::Debug};

use crate::{RuntimeError, Value, ValueRef};

use super::Object;

type Method<T> = fn(&mut T, &[ValueRef]) -> Result<Option<Value>, RuntimeError>;

#[derive(Debug)]
pub struct MetaTable<T> {
    pub name: String,
    pub methods: HashMap<String, MetaMethod<T>>,
    pub properties: HashMap<String, MetaProperty<T>>,
    // _marker: std::marker::PhantomData<T>,
}

impl<T> MetaTable<T> {
    pub fn new(name: impl ToString) -> Self {
        Self {
            name: name.to_string(),
            methods: HashMap::new(),
            properties: HashMap::new(),
            // _marker: std::marker::PhantomData,
        }
    }

    pub fn with_method(mut self, name: impl ToString, method: Method<T>) -> Self {
        self.methods
            .insert(name.to_string(), MetaMethod::new(name, method));
        self
    }

    pub fn add_property(&mut self, property: MetaProperty<T>) {
        self.properties.insert(property.name.clone(), property);
    }

    pub fn with_property_getter(
        mut self,
        name: &str,
        getter: fn(&T) -> Result<Value, RuntimeError>,
    ) -> Self {
        self.properties.insert(
            name.to_string(),
            MetaProperty::new(name, Some(getter), None),
        );
        self
    }

    pub fn with_property_setter(
        mut self,
        name: &str,
        setter: fn(&mut T, Value) -> Result<(), RuntimeError>,
    ) -> Self {
        self.properties.insert(
            name.to_string(),
            MetaProperty::new(name, None, Some(setter)),
        );
        self
    }

    pub fn property_get(&self, this: &T, name: &str) -> Result<Value, RuntimeError> {
        match self.properties.get(name) {
            Some(property) => match property.getter {
                Some(getter) => getter(this),
                None => Err(RuntimeError::missing_property_getter::<T>(name)),
            },
            None => return Err(RuntimeError::missing_property::<T>(name)),
        }
    }

    pub fn property_set(&self, this: &mut T, name: &str, value: Value) -> Result<(), RuntimeError> {
        match self.properties.get(name) {
            Some(property) => match property.setter {
                Some(setter) => setter(this, value),
                None => Err(RuntimeError::missing_property_setter::<T>(name)),
            },
            None => return Err(RuntimeError::missing_property::<T>(name)),
        }
    }

    pub fn method_call(
        &self,
        this: &mut T,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match self.methods.get(method) {
            Some(method) => (method.method)(this, args),
            None => return Err(RuntimeError::missing_method::<T>(method)),
        }
    }
}

#[derive(Debug)]
pub struct MetaMethod<T> {
    pub name: String,
    pub method: fn(&mut T, &[ValueRef]) -> Result<Option<Value>, RuntimeError>,
}

impl<T> MetaMethod<T> {
    pub fn new(
        name: impl ToString,
        method: fn(&mut T, &[ValueRef]) -> Result<Option<Value>, RuntimeError>,
    ) -> Self {
        Self {
            name: name.to_string(),
            method,
        }
    }
}

#[derive(Debug)]
pub struct MetaProperty<T> {
    pub name: String,
    pub getter: Option<fn(&T) -> Result<Value, RuntimeError>>,
    pub setter: Option<fn(&mut T, Value) -> Result<(), RuntimeError>>,
}

impl<T> MetaProperty<T> {
    pub fn new(
        name: &str,
        getter: Option<fn(&T) -> Result<Value, RuntimeError>>,
        setter: Option<fn(&mut T, Value) -> Result<(), RuntimeError>>,
    ) -> Self {
        Self {
            name: name.to_string(),
            getter,
            setter,
        }
    }
}

pub trait MetaObject<T>: std::fmt::Debug + std::any::Any {
    fn get_meta_table(&self) -> &MetaTable<T>;
}

impl<T: 'static> Object for dyn MetaObject<T> {
    fn property_get(&self, member: &str) -> Result<Value, RuntimeError> {
        match self.get_meta_table().properties.get(member) {
            Some(property) => match property.getter {
                Some(getter) => getter((self as &dyn std::any::Any).downcast_ref::<T>().unwrap()),
                None => Err(RuntimeError::missing_property_getter::<T>(member)),
            },
            None => return Err(RuntimeError::missing_property::<T>(member)),
        }
    }

    fn property_set(&mut self, member: &str, value: ValueRef) -> Result<(), RuntimeError> {
        match self.get_meta_table().properties.get(member) {
            Some(property) => match property.setter {
                Some(setter) => setter(
                    (self as &mut dyn std::any::Any)
                        .downcast_mut::<T>()
                        .unwrap(),
                    value.take(),
                ),
                None => Err(RuntimeError::missing_property_setter::<T>(member)),
            },
            None => return Err(RuntimeError::missing_property::<T>(member)),
        }
    }

    fn method_call(
        &mut self,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match self.get_meta_table().methods.get(method) {
            Some(method) => (method.method)(
                (self as &mut dyn std::any::Any)
                    .downcast_mut::<T>()
                    .unwrap(),
                args,
            ),
            None => return Err(RuntimeError::missing_method::<T>(method)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Object;

    #[test]
    fn test_metatable() {
        #[derive(Debug)]
        struct Request {}

        impl MetaObject<Request> for Request {
            fn get_meta_table(&self) -> &MetaTable<Request> {
                static META_TABLE: std::sync::LazyLock<MetaTable<Request>> =
                    std::sync::LazyLock::new(|| {
                        let mut table = MetaTable::new("Request");
                        table.add_property(MetaProperty::new(
                            "method",
                            Some(|_| Ok(Value::new("GET"))),
                            None,
                        ));
                        table
                    });
                &META_TABLE
            }
        }

        let req = Request {};

        let dyn_obj = &req as &dyn MetaObject<Request>;

        let method = dyn_obj.property_get("method").unwrap();

        assert_eq!(method, "GET");
    }
}
