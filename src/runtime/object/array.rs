
use crate::{Object, RuntimeError, Value, ValueRef};

use super::Range;

impl<T: Object + Clone> Object for Vec<T> {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        if let Some(index) = index.downcast_ref::<i64>() {
            if *index >= 0 && *index < self.len() as i64 {
                let item: T = self[*index as usize].clone();
                return Ok(ValueRef::new(item));
            }
            return Err(RuntimeError::IndexOutOfBounds {
                index: *index,
                length: self.len() as i64,
            });
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::IndexGet,
            format!("cannot index with {index:?}"),
        ))
    }

    fn index_set(&mut self, index: &Value, value: ValueRef) -> Result<(), RuntimeError> {
        if let Some(index) = index.downcast_ref::<i64>() {
            if *index >= 0 && *index < self.len() as i64 {
                if let Some(value) = value.downcast_ref::<T>() {
                    self[*index as usize] = value.clone();
                    return Ok(());
                } else {
                    return Err(RuntimeError::invalid_type::<T>(value));
                }
            }
            return Err(RuntimeError::IndexOutOfBounds {
                index: *index,
                length: self.len() as i64,
            });
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::IndexGet,
            format!("cannot index with {index:?}"),
        ))
    }

    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        Ok(Box::new(
            self.clone().into_iter().map(|item| ValueRef::new(item)),
        ))
    }

    fn make_slice(&self, range: ValueRef) -> Result<Value, RuntimeError> {
        if let Some(range) = range.downcast_ref::<Range>() {
            let (start, end) = range.get_range(self.len())?;

            // 创建新的Vec并复制切片内容
            let slice: Vec<T> = self[start..end].to_vec();

            return Ok(Value::new(slice));
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::MakeSlice,
            format!("cannot make_slice with {range:?}"),
        ))
    }

    fn property_call(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match member {
            "len" => Ok(Some(Value::new(self.len() as i64))),
            "enumerate" => {
                let i = self
                    .clone()
                    .into_iter()
                    .enumerate()
                    .map(|(i, item)| (ValueRef::new(i as i64), ValueRef::new(item)))
                    .collect::<Vec<(ValueRef, ValueRef)>>();
                Ok(Some(Value::new(i)))
            }
            _ => Ok(None),
        }
    }
}

impl Object for Vec<ValueRef> {
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        if let Some(index) = index.downcast_ref::<i64>() {
            if *index >= 0 && *index < self.len() as i64 {
                let item = self[*index as usize].clone();
                return Ok(item);
            }
            return Err(RuntimeError::IndexOutOfBounds {
                index: *index,
                length: self.len() as i64,
            });
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::IndexGet,
            format!("cannot index with {index:?}"),
        ))
    }

    fn index_set(&mut self, index: &Value, value: ValueRef) -> Result<(), RuntimeError> {
        if let Some(index) = index.downcast_ref::<i64>() {
            if *index >= 0 && *index < self.len() as i64 {
                self[*index as usize] = value;
                return Ok(());
            }
            return Err(RuntimeError::IndexOutOfBounds {
                index: *index,
                length: self.len() as i64,
            });
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::IndexGet,
            format!("cannot index with {index:?}"),
        ))
    }

    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        Ok(Box::new(self.clone().into_iter()))
    }

    fn make_slice(&self, range: ValueRef) -> Result<Value, RuntimeError> {
        if let Some(range) = range.downcast_ref::<Range>() {
            let (start, end) = range.get_range(self.len())?;

            // 创建新的Vec并复制切片内容
            let slice: Vec<ValueRef> = self[start..end].to_vec();

            return Ok(Value::new(slice));
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::MakeSlice,
            format!("cannot make_slice with {range:?}"),
        ))
    }

    fn property_call(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        match member {
            "len" => Ok(Some(Value::new(self.len() as i64))),
            "enumerate" => {
                let i = self
                    .clone()
                    .into_iter()
                    .enumerate()
                    .map(|(i, item)| (ValueRef::new(i as i64), item))
                    .collect::<Vec<(ValueRef, ValueRef)>>();
                Ok(Some(Value::new(i)))
            }
            _ => Ok(None),
        }
    }
}
