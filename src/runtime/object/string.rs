use crate::{Object, RuntimeError, Value, ValueRef};

use super::{
    Range,
    metatable::{MetaObject, MetaProperty, MetaTable},
};

impl Object for String {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{self}\"")
    }

    fn equal(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<String>() {
            return Ok(Value::new(self == other));
        }

        if let Some(other) = other.downcast_ref::<&str>() {
            return Ok(Value::new(self == other));
        }

        Ok(Value::new(false))
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
    fn method_call(
        &mut self,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        STRING_META_TABLE.method_call(self, method, args)
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

    fn equal(&self, other: &Value) -> Result<Value, RuntimeError> {
        if let Some(other) = other.downcast_ref::<String>() {
            return Ok(Value::new(self == other));
        }

        if let Some(other) = other.downcast_ref::<&str>() {
            return Ok(Value::new(self == other));
        }

        Ok(Value::new(false))
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

static STRING_META_TABLE: std::sync::LazyLock<MetaTable<String>> = std::sync::LazyLock::new(|| {
    MetaTable::new("String")
        .with_method("len", |this: &mut String, args| {
            if args.len() == 0 {
                return Ok(Some(Value::new(this.len())));
            }
            Err(RuntimeError::invalid_argument_count(0, args.len()))
        })
        .with_method("to_uppercase", |this: &mut String, args| {
            if args.len() == 0 {
                return Ok(Some(Value::new(this.to_uppercase())));
            }
            Err(RuntimeError::invalid_argument_count(0, args.len()))
        })
        .with_method("to_lowercase", |this: &mut String, args| {
            if args.len() == 0 {
                return Ok(Some(Value::new(this.to_lowercase())));
            }
            Err(RuntimeError::invalid_argument_count(0, args.len()))
        })
        .with_method("starts_with", |this: &mut String, args| {
            if args.len() == 1 {
                let other = args[0].downcast_ref::<String>().unwrap();
                return Ok(Some(Value::new(this.starts_with(other.as_str()))));
            }
            Err(RuntimeError::invalid_argument_count(1, args.len()))
        })
        .with_method("ends_with", |this: &mut String, args| {
            if args.len() == 1 {
                let other = args[0].downcast_ref::<String>().unwrap();
                return Ok(Some(Value::new(this.ends_with(other.as_str()))));
            }
            Err(RuntimeError::invalid_argument_count(1, args.len()))
        })
        .with_method("contains", |this: &mut String, args| {
            if args.len() == 1 {
                let other = args[0].downcast_ref::<String>().unwrap();
                return Ok(Some(Value::new(this.contains(other.as_str()))));
            }
            Err(RuntimeError::invalid_argument_count(1, args.len()))
        })
        .with_method("replace", |this: &mut String, args| {
            if args.len() == 2 {
                let old = args[0].downcast_ref::<String>().unwrap();
                let new = args[1].downcast_ref::<String>().unwrap();
                return Ok(Some(Value::new(this.replace(old.as_str(), new.as_str()))));
            }
            Err(RuntimeError::invalid_argument_count(2, args.len()))
        })
        .with_method("split", |this: &mut String, args| {
            if args.len() == 1 {
                let delimiter = args[0].downcast_ref::<String>().unwrap();
                return Ok(Some(Value::new(
                    this.split(delimiter.as_str())
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>(),
                )));
            }
            Err(RuntimeError::invalid_argument_count(1, args.len()))
        })
        .with_method("trim", |this: &mut String, args| {
            if args.len() == 0 {
                return Ok(Some(Value::new(this.trim().to_string())));
            }
            Err(RuntimeError::invalid_argument_count(0, args.len()))
        })
        .with_method("trim_start", |this: &mut String, args| {
            if args.len() == 0 {
                return Ok(Some(Value::new(this.trim_start().to_string())));
            }
            Err(RuntimeError::invalid_argument_count(0, args.len()))
        })
        .with_method("trim_end", |this: &mut String, args| {
            if args.len() == 0 {
                return Ok(Some(Value::new(this.trim_end().to_string())));
            }
            Err(RuntimeError::invalid_argument_count(0, args.len()))
        })
        .with_method("chars", |this: &mut String, args| {
            if args.len() == 0 {
                return Ok(Some(Value::new(this.chars().collect::<Vec<char>>())));
            }
            Err(RuntimeError::invalid_argument_count(0, args.len()))
        })
});
