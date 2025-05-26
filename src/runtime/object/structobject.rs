use std::collections::HashMap;

use crate::ValueRef;

use super::Object;

#[derive(Debug, Clone)]
pub struct StructObject {
    fields: HashMap<String, ValueRef>,
}

impl StructObject {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }

    pub fn make_field(&mut self, name: impl ToString, value: ValueRef) {
        self.fields.insert(name.to_string(), value);
    }
}

impl Object for StructObject {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "StructObject")
    }

    fn property_get(&self, property: &str) -> Result<crate::ValueRef, crate::RuntimeError> {
        Ok(self.fields.get(property).unwrap().clone())
    }

    fn property_set(&mut self, property: &str, value: ValueRef) -> Result<(), crate::RuntimeError> {
        self.fields.insert(property.to_string(), value);
        Ok(())
    }
}
