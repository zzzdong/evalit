use crate::{Object, RuntimeError, Value};

macro_rules! impl_object_for_integer {
    ($ty:ty) => {
        impl Object for $ty {
            fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self)
            }

            
            fn equal(&self, other: &Value) -> Result<Value, RuntimeError> {
                if let Some(other) = other.downcast_ref::<$ty>() {
                    return Ok(Value::new(*self == *other));
                }

                if let Some(other) = other.downcast_ref::<i64>() {
                    return Ok(Value::new(*self as i64 == *other));
                }

                Ok(Value::new(false))
            }

            fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
                match other.downcast_ref::<$ty>() {
                    Some(other) => Ok(self.cmp(&other)),
                    None => Err(RuntimeError::invalid_type::<$ty>(other)),
                }
            }

            fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
                match other.downcast_ref::<$ty>() {
                    Some(other) => match self.checked_add(*other) {
                        Some(result) => Ok(Value::new(result)),
                        None => Err(RuntimeError::Overflow),
                    },
                    None => Err(RuntimeError::invalid_type::<$ty>(other)),
                }
            }

            fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
                match other.downcast_ref::<$ty>() {
                    Some(other) => match self.checked_sub(*other) {
                        Some(result) => Ok(Value::new(result)),
                        None => Err(RuntimeError::Overflow),
                    },
                    None => Err(RuntimeError::invalid_type::<$ty>(other)),
                }
            }

            fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
                match other.downcast_ref::<$ty>() {
                    Some(other) => match self.checked_mul(*other) {
                        Some(result) => Ok(Value::new(result)),
                        None => Err(RuntimeError::Overflow),
                    },
                    None => Err(RuntimeError::invalid_type::<$ty>(other)),
                }
            }

            fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
                match other.downcast_ref::<$ty>() {
                    Some(other) => match self.checked_div(*other) {
                        Some(result) => Ok(Value::new(result)),
                        None => Err(RuntimeError::Overflow),
                    },
                    None => Err(RuntimeError::invalid_type::<$ty>(other)),
                }
            }

            fn rem(&self, other: &Value) -> Result<Value, RuntimeError> {
                match other.downcast_ref::<$ty>() {
                    Some(other) => match self.checked_rem(*other) {
                        Some(result) => Ok(Value::new(result)),
                        None => Err(RuntimeError::Overflow),
                    },
                    None => Err(RuntimeError::invalid_type::<$ty>(other)),
                }
            }

            fn negate(&self) -> Result<Value, RuntimeError> {
                match self.checked_neg() {
                    Some(result) => Ok(Value::new(result)),
                    None => Err(RuntimeError::Overflow),
                }
            }
        }
    };
}

// 为指定的整数类型实现 Object Trait
impl_object_for_integer!(u8);
impl_object_for_integer!(u16);
impl_object_for_integer!(u32);
impl_object_for_integer!(u64);
impl_object_for_integer!(usize);
impl_object_for_integer!(i8);
impl_object_for_integer!(i16);
impl_object_for_integer!(i32);
impl_object_for_integer!(i64);
impl_object_for_integer!(isize);

// impl PartialEq<i32> for Value {
//     fn eq(&self, other: &i32) -> bool {
//         match self.downcast_ref::<i32>() {
//             Some(v) => v == other,
//             None => match self.downcast_ref::<i64>() {
//                 Some(v) => *v as i32 == *other,
//                 None => false,
//             },
//         }
//     }
// }

macro_rules! impl_partial_eq_integer {
    ($i:ty) => {
        impl PartialEq<$i> for Value {
            fn eq(&self, other: &$i) -> bool {
                match self.downcast_ref::<$i>() {
                    Some(v) => *v == *other,
                    None => match self.downcast_ref::<i64>() {
                        Some(v) => *v as $i == *other,
                        None => false,
                    },
                }
            }
        }
    };
}

impl_partial_eq_integer!(u8);
impl_partial_eq_integer!(u16);
impl_partial_eq_integer!(u32);
impl_partial_eq_integer!(u64);
impl_partial_eq_integer!(usize);
impl_partial_eq_integer!(i8);
impl_partial_eq_integer!(i16);
impl_partial_eq_integer!(i32);
impl_partial_eq_integer!(i64);
impl_partial_eq_integer!(isize);
