use crate::{RuntimeError, Value, ValueRef};

use super::Object;

impl Object for Option<ValueRef> {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Option")
    }

    fn method_call(
        &mut self,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        match method {
            "is_some" => {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }

                Ok(Some(ValueRef::new(self.is_some())))
            }
            "is_none" => {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }
                Ok(Some(ValueRef::new(self.is_none())))
            }
            "unwrap" => {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }

                match self {
                    Some(value) => Ok(Some(value.clone())),
                    None => Err(RuntimeError::internal("can not unwrap a None")),
                }
            }
            _ => Err(RuntimeError::missing_method::<Self>(method)),
        }
    }
}

impl<T> Object for Option<T>
where
    T: Object + Clone,
{
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Option")
    }

    fn method_call(
        &mut self,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        match method {
            "is_some" => {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }

                Ok(Some(ValueRef::new(self.is_some())))
            }
            "is_none" => {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }
                Ok(Some(ValueRef::new(self.is_none())))
            }
            "unwrap" => {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }

                match self {
                    Some(value) => Ok(Some(ValueRef::new(value.clone()))),
                    None => Err(RuntimeError::internal("can not unwrap a None")),
                }
            }
            _ => Err(RuntimeError::missing_method::<Self>(method)),
        }
    }
}

impl<T: Object + Clone + PartialEq> PartialEq<Option<T>> for Value {
    fn eq(&self, other: &Option<T>) -> bool {
        match self.downcast_ref::<Option<T>>() {
            Some(value) => *value == *other,
            None => false,
        }
    }
}
impl<T: Object + Clone + PartialEq> PartialEq<Value> for Option<T> {
    fn eq(&self, other: &Value) -> bool {
        match other.downcast_ref::<Option<T>>() {
            Some(value) => *self == *value,
            None => false,
        }
    }
}
