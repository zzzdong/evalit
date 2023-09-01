use crate::{Object, RuntimeError, Value, ValueRef};

use super::Range;

impl Object for String {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{self}\"")
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        if let Some(other) = other.downcast_ref::<String>() {
            return Ok(self.cmp(other));
        }

        if let Some(other) = other.downcast_ref::<&str>() {
            return Ok(self.as_str().cmp(other));
        }

        Err(RuntimeError::invalid_type::<String>(other))
    }

    fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<String>() {
            return Ok(Value::new(self.to_string() + other));
        }

        if let Some(other) = other.downcast_ref::<&str>() {
            return Ok(Value::new(self.to_string() + other));
        }

        Err(RuntimeError::invalid_type::<String>(other))
    }

    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        let chars = self
            .chars()
            .clone()
            .map(ValueRef::new)
            .collect::<Vec<ValueRef>>();

        let iter = chars.into_iter();

        Ok(Box::new(iter))
    }

    fn make_slice(&self, range: ValueRef) -> Result<Value, RuntimeError> {
        if let Some(range) = range.downcast_ref::<Range>() {
            let len = self.chars().count();

            let (start, end) = range.get_range(len)?;

            let slice = self
                .chars()
                .skip(start)
                .take(end - start)
                .collect::<String>();

            return Ok(Value::new(slice));
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::MakeSlice,
            format!("cannot make_slice with {range:?}"),
        ))
    }
}

impl PartialEq<String> for Value {
    fn eq(&self, other: &String) -> bool {
        match self.downcast_ref::<String>() {
            Some(value) => *value == *other,
            None => match self.downcast_ref::<&str>() {
                Some(value) => *value == *other,
                None => false,
            },
        }
    }
}

impl Object for &'static str {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{self}\"")
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        if let Some(other) = other.downcast_ref::<&str>() {
            return Ok(self.cmp(other));
        }

        if let Some(other) = other.downcast_ref::<String>() {
            return Ok(self.cmp(&other.as_str()));
        }

        Err(RuntimeError::invalid_type::<String>(other))
    }
}

impl PartialEq<&'static str> for Value {
    fn eq(&self, other: &&'static str) -> bool {
        match self.downcast_ref::<String>() {
            Some(value) => *value == *other,
            None => match self.downcast_ref::<&str>() {
                Some(value) => *value == *other,
                None => false,
            },
        }
    }
}

impl Object for char {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        if let Some(other) = other.downcast_ref::<char>() {
            return Ok(self.cmp(other));
        }

        Err(RuntimeError::invalid_type::<char>(other))
    }
}

impl PartialEq<char> for Value {
    fn eq(&self, other: &char) -> bool {
        match self.downcast_ref::<char>() {
            Some(value) => *value == *other,
            None => false,
        }
    }
}
