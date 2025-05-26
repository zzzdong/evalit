use crate::RuntimeError;

use super::{Object, Value};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Null;

impl Object for Null {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("null")
    }

    fn equal(&self, other: &Value) -> Result<Value, RuntimeError> {
        match other.downcast_ref::<Null>() {
            Some(_) => Ok(Value::new(true)),
            None => Ok(Value::new(false)),
        }
    }
}

impl PartialEq<Null> for Value {
    fn eq(&self, other: &Null) -> bool {
        match self.downcast_ref::<Null>() {
            Some(value) => *value == *other,
            None => false,
        }
    }
}
