use crate::{Object, RuntimeError, Value};

impl Object for f32 {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }

    fn equal(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<f32>() {
            return Ok(Value::new(*self == *other));
        }

        if let Some(other) = other.downcast_ref::<f64>() {
            return Ok(Value::new(*self as f64 == *other));
        }

        Ok(Value::new(false))
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

    fn method_call(
        &mut self,
        method: &str,
        args: &[crate::ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match method {
            "to_string" => Ok(Some(Value::new(self.to_string()))),
            "to_int" => Ok(Some(Value::new(*self as i64))),
            "abs" => Ok(Some(Value::new(self.abs()))),
            "round" => Ok(Some(Value::new(self.round()))),
            "floor" => Ok(Some(Value::new(self.floor()))),
            "ceil" => Ok(Some(Value::new(self.ceil()))),
            "trunc" => Ok(Some(Value::new(self.trunc()))),
            "sqrt" => Ok(Some(Value::new(self.sqrt()))),
            "sin" => Ok(Some(Value::new(self.sin()))),
            "cos" => Ok(Some(Value::new(self.cos()))),
            "tan" => Ok(Some(Value::new(self.tan()))),
            "asin" => Ok(Some(Value::new(self.asin()))),
            "acos" => Ok(Some(Value::new(self.acos()))),
            "atan" => Ok(Some(Value::new(self.atan()))),
            "log" => {
                if args.len() == 1 {
                    match args[0].downcast_ref::<f32>() {
                        Some(base) => Ok(Some(Value::new(self.log(*base)))),
                        None => Err(RuntimeError::invalid_argument::<f32>(0, &args[0])),
                    }
                } else {
                    Err(RuntimeError::invalid_argument_count(1, args.len()))
                }
            }

            _ => Err(RuntimeError::invalid_operation(
                super::OperateKind::PropertyCall,
                format!("Unknown method: {}", method),
            )),
        }
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

    fn method_call(
        &mut self,
        method: &str,
        args: &[crate::ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match method {
            "to_string" => Ok(Some(Value::new(self.to_string()))),
            "to_int" => Ok(Some(Value::new(*self as i64))),
            "abs" => Ok(Some(Value::new(self.abs()))),
            "round" => Ok(Some(Value::new(self.round()))),
            "floor" => Ok(Some(Value::new(self.floor()))),
            "ceil" => Ok(Some(Value::new(self.ceil()))),
            "trunc" => Ok(Some(Value::new(self.trunc()))),
            "sqrt" => Ok(Some(Value::new(self.sqrt()))),
            "sin" => Ok(Some(Value::new(self.sin()))),
            "cos" => Ok(Some(Value::new(self.cos()))),
            "tan" => Ok(Some(Value::new(self.tan()))),
            "asin" => Ok(Some(Value::new(self.asin()))),
            "acos" => Ok(Some(Value::new(self.acos()))),
            "atan" => Ok(Some(Value::new(self.atan()))),
            "log" => {
                if args.len() == 1 {
                    match args[0].downcast_ref::<f64>() {
                        Some(base) => Ok(Some(Value::new(self.log(*base)))),
                        None => Err(RuntimeError::invalid_argument::<f64>(0, &args[0])),
                    }
                } else {
                    Err(RuntimeError::invalid_argument_count(1, args.len()))
                }
            }
            _ => Err(RuntimeError::invalid_operation(
                super::OperateKind::PropertyCall,
                format!("Unknown method: {}", method),
            )),
        }
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

impl PartialEq<f64> for Value {
    fn eq(&self, other: &f64) -> bool {
        match self.downcast_ref::<f64>() {
            Some(value) => *value == *other,
            None => false,
        }
    }
}
