use crate::{Object, RuntimeError, Value};

impl Object for bool {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        if let Some(other) = other.downcast_ref::<bool>() {
            return Ok(self.cmp(other));
        }

        Err(RuntimeError::invalid_type::<bool>(other))
    }

    fn logic_and(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<bool>() {
            return Ok(Value::new(*self && *other));
        }
        Err(RuntimeError::invalid_type::<bool>(other))
    }

    fn logic_or(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<bool>() {
            return Ok(Value::new(*self || *other));
        }
        Err(RuntimeError::invalid_type::<bool>(other))
    }

    fn negate(&self) -> Result<Value, RuntimeError> {
        Ok(Value::new(!*self))
    }
}

impl PartialEq<bool> for Value {
    fn eq(&self, other: &bool) -> bool {
        match self.downcast_ref::<bool>() {
            Some(value) => *value == *other,
            None => false,
        }
    }
}
