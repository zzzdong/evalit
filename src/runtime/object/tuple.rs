use std::fmt;

use crate::{Object, RuntimeError, Value, ValueRef};

use super::OperateKind;

/// ()
impl Object for () {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "()")
    }

    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        let other = other
            .downcast_ref::<()>()
            .ok_or(RuntimeError::invalid_type::<()>(other))?;
        self.partial_cmp(other).ok_or_else(|| {
            RuntimeError::invalid_operation(
                OperateKind::Compare,
                format!("can not compare () with {other:?}"),
            )
        })
    }
}

impl Object for (ValueRef, ValueRef) {
    fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?}, {:?})", self.0, self.1)
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        let value = index
            .downcast_ref::<i64>()
            .ok_or(RuntimeError::invalid_type::<i64>(index))?;

        match *value {
            0 => Ok(self.0.clone()),
            1 => Ok(self.1.clone()),
            _ => Err(RuntimeError::IndexOutOfBounds {
                index: *value,
                length: 2,
            }),
        }
    }
}


// impl Object for (ValueRef, ValueRef) {
//     fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "({:?}, {:?})", self.0, self.1)
//     }

//     fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
//         let value = index
//             .downcast_ref::<i64>()
//             .ok_or(RuntimeError::invalid_type::<i64>(index))?;

//         match *value {
//             0 => Ok(self.0.clone()),
//             1 => Ok(self.1.clone()),
//             _ => Err(RuntimeError::IndexOutOfBounds {
//                 index: *value,
//                 length: 2,
//             }),
//         }
//     }
// }

// impl<T: Object + Clone> Object for (T, Value) {
//     fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "({:?}, {:?})", self.0, self.1)
//     }

//     fn index_get(&self, index: &Value) -> Result<Value, RuntimeError> {
//         let value = index
//             .downcast_ref::<i64>()
//             .ok_or(RuntimeError::invalid_type::<i64>(index))?;

//         match *value {
//             0 => Ok(Value::new(self.0.clone())),
//             1 => Ok(self.1.clone()),
//             _ => Err(RuntimeError::IndexOutOfBounds {
//                 index: *value,
//                 length: 2,
//             }),
//         }
//     }
// }

// impl Object for (usize, Value) {
//     fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "({}, {})", self.0, self.1)
//     }

//     fn index_get(&self, index: &Value) -> Result<Value, RuntimeError> {
//         let value = index
//             .downcast_ref::<i64>()
//             .ok_or(RuntimeError::invalid_type::<i64>(index))?;

//         match *value {
//             0 => Ok(Value::new(self.0.clone())),
//             1 => Ok(self.1.clone()),
//             _ => Err(RuntimeError::IndexOutOfBounds {
//                 index: *value,
//                 length: 2,
//             }),
//         }
//     }
// }

impl<T1, T2> Object for (T1, T2)
where
    T1: Object + Clone,
    T2: Object + Clone,
{
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "()")
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        let value = index
            .downcast_ref::<i64>()
            .ok_or(RuntimeError::invalid_type::<i64>(index))?;

        match *value {
            0 => Ok(ValueRef::new(self.0.clone())),
            1 => Ok(ValueRef::new(self.1.clone())),
            _ => Err(RuntimeError::IndexOutOfBounds {
                index: *value,
                length: 2,
            }),
        }
    }
}
