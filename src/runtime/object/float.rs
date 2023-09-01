use crate::{Object, RuntimeError, Value};

impl Object for f32 {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f32>() {
            return self
                .partial_cmp(other)
                .ok_or(RuntimeError::invalid_operation(
                    super::OperateKind::Compare,
                    format!("cannot compare float {self} with {other}"),
                ));
        }

        Err(RuntimeError::invalid_type::<f32>(other))
    }

    fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f32>() {
            return Ok(Value::new(self + *other));
        }

        Err(RuntimeError::invalid_type::<f32>(other))
    }

    fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f32>() {
            return Ok(Value::new(self - *other));
        }

        Err(RuntimeError::invalid_type::<f32>(other))
    }

    fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f32>() {
            return Ok(Value::new(self * *other));
        }

        Err(RuntimeError::invalid_type::<f32>(other))
    }

    fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f32>() {
            return Ok(Value::new(self / *other));
        }

        Err(RuntimeError::invalid_type::<f32>(other))
    }

    fn negate(&self) -> Result<Value, RuntimeError> {
        Ok(Value::new(-self))
    }
}

impl Object for f64 {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f64>() {
            return self
                .partial_cmp(other)
                .ok_or(RuntimeError::invalid_operation(
                    super::OperateKind::Compare,
                    format!("cannot compare float {self} with {other}"),
                ));
        }
        Err(RuntimeError::invalid_type::<f64>(other))
    }

    fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f64>() {
            return Ok(Value::new(self + *other));
        }

        Err(RuntimeError::invalid_type::<f64>(other))
    }

    fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f64>() {
            return Ok(Value::new(self - *other));
        }

        Err(RuntimeError::invalid_type::<f64>(other))
    }

    fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f64>() {
            return Ok(Value::new(self * *other));
        }

        Err(RuntimeError::invalid_type::<f64>(other))
    }

    fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f64>() {
            return Ok(Value::new(self / *other));
        }

        Err(RuntimeError::invalid_type::<f64>(other))
    }

    fn negate(&self) -> Result<Value, RuntimeError> {
        Ok(Value::new(-self))
    }
}

impl PartialEq<f32> for Value {
    fn eq(&self, other: &f32) -> bool {
        match self.downcast_ref::<f32>() {
            Some(value) => *value == *other,
            None => false,
        }
    }
}
