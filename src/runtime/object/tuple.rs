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

macro_rules! impl_object_for_tuple {
    ($n: expr, $($idx: tt => $t: ident),+) => {
        impl Object for ($($t,)*) {
            fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "({:?}, {:?})", self.0, self.1)
            }

            fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
                let value = index
                    .downcast_ref::<i64>()
                    .ok_or(RuntimeError::invalid_type::<i64>(index))?;

                match *value {
                    $(
                        $idx => Ok(self.$idx.clone()),
                    )*
                    _ => Err(RuntimeError::IndexOutOfBounds {
                        index: *value,
                        length: $n,
                    }),
                }
            }
        }
    };
}

impl_object_for_tuple!(2, 0=>ValueRef, 1=>ValueRef);
impl_object_for_tuple!(3, 0=>ValueRef, 1=>ValueRef, 2=>ValueRef);
impl_object_for_tuple!(4, 0=>ValueRef, 1=>ValueRef, 2=>ValueRef, 3=>ValueRef);
impl_object_for_tuple!(5, 0=>ValueRef, 1=>ValueRef, 2=>ValueRef, 3=>ValueRef, 4=>ValueRef);
impl_object_for_tuple!(6, 0=>ValueRef, 1=>ValueRef, 2=>ValueRef, 3=>ValueRef, 4=>ValueRef, 5=>ValueRef);
impl_object_for_tuple!(7, 0=>ValueRef, 1=>ValueRef, 2=>ValueRef, 3=>ValueRef, 4=>ValueRef, 5=>ValueRef, 6=>ValueRef);
impl_object_for_tuple!(8, 0=>ValueRef, 1=>ValueRef, 2=>ValueRef, 3=>ValueRef, 4=>ValueRef, 5=>ValueRef, 6=>ValueRef, 7=>ValueRef);
impl_object_for_tuple!(9, 0=>ValueRef, 1=>ValueRef, 2=>ValueRef, 3=>ValueRef, 4=>ValueRef, 5=>ValueRef, 6=>ValueRef, 7=>ValueRef, 8=>ValueRef);
impl_object_for_tuple!(10, 0=>ValueRef, 1=>ValueRef, 2=>ValueRef, 3=>ValueRef, 4=>ValueRef, 5=>ValueRef, 6=>ValueRef, 7=>ValueRef, 8=>ValueRef, 9=>ValueRef);
impl_object_for_tuple!(11, 0=>ValueRef, 1=>ValueRef, 2=>ValueRef, 3=>ValueRef, 4=>ValueRef, 5=>ValueRef, 6=>ValueRef, 7=>ValueRef, 8=>ValueRef, 9=>ValueRef, 10=>ValueRef);
impl_object_for_tuple!(12, 0=>ValueRef, 1=>ValueRef, 2=>ValueRef, 3=>ValueRef, 4=>ValueRef, 5=>ValueRef, 6=>ValueRef, 7=>ValueRef, 8=>ValueRef, 9=>ValueRef, 10=>ValueRef, 11=>ValueRef);

macro_rules! impl_object_for_object_tuple {
    ($n: expr, $($idx: tt => $t: ident),+) => {
        impl<$($t,)*> Object for ($($t,)*)
        where
            $($t: Object + Clone),*
        {
            fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "({:?}, {:?})", self.0, self.1)
            }

            fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
                let value = index
                    .downcast_ref::<i64>()
                    .ok_or(RuntimeError::invalid_type::<i64>(index))?;

                match *value {
                    $(
                        $idx => Ok(ValueRef::new(self.$idx.clone())),
                    )*
                    _ => Err(RuntimeError::IndexOutOfBounds {
                        index: *value,
                        length: $n,
                    }),
                }
            }
        }
    };
}

impl_object_for_object_tuple!(2, 0 => T0, 1 => T1);
impl_object_for_object_tuple!(3, 0 => T0, 1 => T1, 2 => T2);
impl_object_for_object_tuple!(4, 0 => T0, 1 => T1, 2 => T2, 3 => T3);
impl_object_for_object_tuple!(5, 0 => T0, 1 => T1, 2 => T2, 3 => T3, 4 => T4);
impl_object_for_object_tuple!(6, 0 => T0, 1 => T1, 2 => T2, 3 => T3, 4 => T4, 5 => T5);
impl_object_for_object_tuple!(7, 0 => T0, 1 => T1, 2 => T2, 3 => T3, 4 => T4, 5 => T5, 6 => T6);
impl_object_for_object_tuple!(8, 0 => T0, 1 => T1, 2 => T2, 3 => T3, 4 => T4, 5 => T5, 6 => T6, 7 => T7);
impl_object_for_object_tuple!(9, 0 => T0, 1 => T1, 2 => T2, 3 => T3, 4 => T4, 5 => T5, 6 => T6, 7 => T7, 8 => T8);
impl_object_for_object_tuple!(10, 0 => T0, 1 => T1, 2 => T2, 3 => T3, 4 => T4, 5 => T5, 6 => T6, 7 => T7, 8 => T8, 9 => T9);
impl_object_for_object_tuple!(11, 0 => T0, 1 => T1, 2 => T2, 3 => T3, 4 => T4, 5 => T5, 6 => T6, 7 => T7, 8 => T8, 9 => T9, 10 => T10);
impl_object_for_object_tuple!(12, 0 => T0, 1 => T1, 2 => T2, 3 => T3, 4 => T4, 5 => T5, 6 => T6, 7 => T7, 8 => T8, 9 => T9, 10 => T10, 11 => T11);
