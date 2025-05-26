use crate::{Object, RuntimeError, Value, ValueRef};

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

    fn call_method(
        &mut self,
        method: &str,
        args: &[crate::ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        match method {
            "to_string" => Ok(Some(ValueRef::new(self.to_string()))),
            "to_int" => Ok(Some(ValueRef::new(*self as i64))),
            "abs" => Ok(Some(ValueRef::new(self.abs()))),
            "round" => Ok(Some(ValueRef::new(self.round()))),
            "floor" => Ok(Some(ValueRef::new(self.floor()))),
            "ceil" => Ok(Some(ValueRef::new(self.ceil()))),
            "trunc" => Ok(Some(ValueRef::new(self.trunc()))),
            "sqrt" => Ok(Some(ValueRef::new(self.sqrt()))),
            "sin" => Ok(Some(ValueRef::new(self.sin()))),
            "cos" => Ok(Some(ValueRef::new(self.cos()))),
            "tan" => Ok(Some(ValueRef::new(self.tan()))),
            "asin" => Ok(Some(ValueRef::new(self.asin()))),
            "acos" => Ok(Some(ValueRef::new(self.acos()))),
            "atan" => Ok(Some(ValueRef::new(self.atan()))),
            "log" => {
                if args.len() == 1 {
                    match args[0].value().downcast_ref::<f32>() {
                        Some(base) => Ok(Some(ValueRef::new(self.log(*base)))),
                        None => Err(RuntimeError::invalid_argument::<f32>(0, &args[0])),
                    }
                } else {
                    Err(RuntimeError::invalid_argument_count(1, args.len()))
                }
            }

            _ => Err(RuntimeError::invalid_operation(
                super::OperateKind::PropertyCall,
                format!("Unknown method: {method}"),
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

    fn call_method(
        &mut self,
        method: &str,
        args: &[crate::ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        match method {
            "to_string" => Ok(Some(ValueRef::new(self.to_string()))),
            "to_int" => Ok(Some(ValueRef::new(*self as i64))),
            "abs" => Ok(Some(ValueRef::new(self.abs()))),
            "round" => Ok(Some(ValueRef::new(self.round()))),
            "floor" => Ok(Some(ValueRef::new(self.floor()))),
            "ceil" => Ok(Some(ValueRef::new(self.ceil()))),
            "trunc" => Ok(Some(ValueRef::new(self.trunc()))),
            "sqrt" => Ok(Some(ValueRef::new(self.sqrt()))),
            "sin" => Ok(Some(ValueRef::new(self.sin()))),
            "cos" => Ok(Some(ValueRef::new(self.cos()))),
            "tan" => Ok(Some(ValueRef::new(self.tan()))),
            "asin" => Ok(Some(ValueRef::new(self.asin()))),
            "acos" => Ok(Some(ValueRef::new(self.acos()))),
            "atan" => Ok(Some(ValueRef::new(self.atan()))),
            "log" => {
                if args.len() == 1 {
                    match args[0].value().downcast_ref::<f64>() {
                        Some(base) => Ok(Some(ValueRef::new(self.log(*base)))),
                        None => Err(RuntimeError::invalid_argument::<f64>(0, &args[0])),
                    }
                } else {
                    Err(RuntimeError::invalid_argument_count(1, args.len()))
                }
            }
            _ => Err(RuntimeError::invalid_operation(
                super::OperateKind::PropertyCall,
                format!("Unknown method: {method}"),
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
