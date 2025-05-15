use crate::{Object, RuntimeError, Value, ValueRef};

use super::{Enumerator, Range, metatable::MetaTable};

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

    fn method_call(
        &mut self,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        match method {
            "len" => Ok(Some(ValueRef::new(self.len() as i64))),
            "clear" => {
                self.clear();
                Ok(None)
            }
            "enumerate" => {
                let i = self
                    .clone()
                    .into_iter()
                    .enumerate()
                    .map(|(i, item)| (ValueRef::new(i as i64), ValueRef::new(item)))
                    .collect::<Vec<(ValueRef, ValueRef)>>();
                Ok(Some(ValueRef::new(i)))
            }
            "push" => {
                if args.len() == 1 {
                    return match args[0].downcast_ref::<T>() {
                        Some(item) => {
                            self.push(item.clone());
                            Ok(None)
                        }
                        None => Err(RuntimeError::invalid_argument::<T>(0, &args[0])),
                    };
                }
                Err(RuntimeError::invalid_argument_count(1, args.len()))
            }
            "pop" => {
                if args.is_empty() {
                    if let Some(item) = self.pop() {
                        return Ok(Some(ValueRef::new(item)));
                    }
                    return Ok(None);
                }
                Err(RuntimeError::invalid_argument_count(0, args.len()))
            }
            "remove" => {
                if args.len() == 1 {
                    if let Some(index) = args[0].downcast_ref::<i64>() {
                        if *index >= 0 && *index < self.len() as i64 {
                            let removed = self.remove(*index as usize);
                            return Ok(Some(ValueRef::new(removed)));
                        }
                        return Err(RuntimeError::IndexOutOfBounds {
                            length: self.len() as i64,
                            index: *index,
                        });
                    }
                    return Err(RuntimeError::invalid_argument::<i64>(
                        0,
                        format!("cannot remove with {:?}", args[0]),
                    ));
                }

                Err(RuntimeError::invalid_argument_count(1, args.len()))
            }
            _ => Err(RuntimeError::missing_method::<Self>(method)),
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

    fn method_call(
        &mut self,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        ARRAY_METATABLE.method_call(self, method, args)
    }
}

static ARRAY_METATABLE: std::sync::LazyLock<MetaTable<Vec<ValueRef>>> =
    std::sync::LazyLock::new(|| {
        MetaTable::new("array")
            .with_method("len", |this: &mut Vec<ValueRef>, args| {
                if args.is_empty() {
                    return Ok(Some(ValueRef::new(this.len() as i64)));
                }
                Err(RuntimeError::invalid_argument_count(0, args.len()))
            })
            .with_method("clear", |this: &mut Vec<ValueRef>, args| {
                if args.is_empty() {
                    this.clear();
                    return Ok(None);
                }
                Err(RuntimeError::invalid_argument_count(0, args.len()))
            })
            .with_method("push", |this: &mut Vec<ValueRef>, args| {
                if args.len() == 1 {
                    this.push(args[0].clone());
                    return Ok(None);
                }
                Err(RuntimeError::invalid_argument_count(1, args.len()))
            })
            .with_method("pop", |this: &mut Vec<ValueRef>, args| {
                if args.is_empty() {
                    if let Some(value) = this.pop() {
                        return Ok(Some(value));
                    }
                    return Ok(None);
                }
                Err(RuntimeError::invalid_argument_count(0, args.len()))
            })
            .with_method("remove", |this: &mut Vec<ValueRef>, args| {
                if args.len() == 1 {
                    if let Some(index) = args[0].downcast_ref::<i64>() {
                        if *index >= 0 && *index < this.len() as i64 {
                            let removed = this.remove(*index as usize);
                            return Ok(Some(removed));
                        }
                        return Err(RuntimeError::IndexOutOfBounds {
                            length: this.len() as i64,
                            index: *index,
                        });
                    }
                    return Err(RuntimeError::invalid_argument::<i64>(
                        0,
                        format!("cannot remove with {:?}", args[0]),
                    ));
                }

                Err(RuntimeError::invalid_argument_count(1, args.len()))
            })
            .with_method("iter", |this: &mut Vec<ValueRef>, args| {
                if args.is_empty() {
                    let iter = this.clone().into_iter();
                    return Ok(Some(ValueRef::new(Enumerator::new(Box::new(iter)))));
                }
                Err(RuntimeError::invalid_argument_count(0, args.len()))
            })
    });
