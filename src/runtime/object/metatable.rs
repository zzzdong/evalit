use std::{collections::HashMap, fmt::Debug};

use crate::{RuntimeError, Value, ValueRef};

use super::Object;

type Method<T> = fn(&mut T, &[ValueRef]) -> Result<Option<ValueRef>, RuntimeError>;
type Getter<T> = fn(&T) -> Result<Value, RuntimeError>;
type Setter<T> = fn(&mut T, Value) -> Result<(), RuntimeError>;

#[derive(Debug)]
pub struct MetaTable<T> {
    pub name: String,
    pub methods: HashMap<String, Method<T>>,
    pub properties: HashMap<String, MetaProperty<T>>,
}

impl<T> MetaTable<T> {
    pub fn new(name: impl ToString) -> Self {
        Self {
            name: name.to_string(),
            methods: HashMap::new(),
            properties: HashMap::new(),
        }
    }

    pub fn with_method(mut self, name: impl ToString, method: Method<T>) -> Self {
        self.methods.insert(name.to_string(), method);
        self
    }

    pub fn add_property(&mut self, property: MetaProperty<T>) {
        self.properties.insert(property.name.clone(), property);
    }

    pub fn with_property_getter(mut self, name: &str, getter: Getter<T>) -> Self {
        self.properties.insert(
            name.to_string(),
            MetaProperty::new(name, Some(getter), None),
        );
        self
    }

    pub fn with_property_setter(mut self, name: &str, setter: Setter<T>) -> Self {
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
            None => Err(RuntimeError::missing_property::<T>(name)),
        }
    }

    pub fn property_set(&self, this: &mut T, name: &str, value: Value) -> Result<(), RuntimeError> {
        match self.properties.get(name) {
            Some(property) => match property.setter {
                Some(setter) => setter(this, value),
                None => Err(RuntimeError::missing_property_setter::<T>(name)),
            },
            None => Err(RuntimeError::missing_property::<T>(name)),
        }
    }

    pub fn method_call(
        &self,
        this: &mut T,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        match self.methods.get(method) {
            Some(method_fn) => (method_fn)(this, args),
            None => Err(RuntimeError::missing_method::<T>(method)),
        }
    }
}

#[derive(Debug)]
pub struct MetaProperty<T> {
    pub name: String,
    pub getter: Option<Getter<T>>,
    pub setter: Option<Setter<T>>,
}

impl<T> MetaProperty<T> {
    pub fn new(name: &str, getter: Option<Getter<T>>, setter: Option<Setter<T>>) -> Self {
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
            None => Err(RuntimeError::missing_property::<T>(member)),
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
            None => Err(RuntimeError::missing_property::<T>(member)),
        }
    }

    fn method_call(
        &mut self,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        match self.get_meta_table().methods.get(method) {
            Some(method) => (method)(
                (self as &mut dyn std::any::Any)
                    .downcast_mut::<T>()
                    .unwrap(),
                args,
            ),
            None => Err(RuntimeError::missing_method::<T>(method)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
