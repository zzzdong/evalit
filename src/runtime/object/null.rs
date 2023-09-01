use crate::RuntimeError;

use super::{Object, Value};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Null;

impl Object for Null {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("null")
    }

    fn compare(&self, other: &super::Value) -> Result<std::cmp::Ordering, crate::RuntimeError> {
        if let Some(other) = other.downcast_ref::<Self>() {
            return Ok(self.partial_cmp(other).unwrap());
        }
        // if let Some(other) = other.downcast_ref::<Option<_>>() {
        //     return Ok(std::cmp::Ordering::Equal);
        // }

        Err(RuntimeError::invalid_type::<Null>(other))
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
