use std::collections::HashMap;

use crate::{Object, RuntimeError, Value, ValueRef};

impl<K, V> Object for HashMap<K, V>
where
    K: Object + std::hash::Hash + std::cmp::Eq + Clone,
    V: Object + Clone,
{
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        if let Some(key) = index.downcast_ref::<K>() {
            if let Some(value) = self.get(key) {
                return Ok(ValueRef::new(value.clone()));
            }

            return Ok(ValueRef::null());
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::IndexGet,
            format!("cannot index hashmap with {index:?}"),
        ))
    }

    fn index_set(&mut self, index: &Value, value: ValueRef) -> Result<(), RuntimeError> {
        if let Some(key) = index.downcast_ref::<K>() {
            if let Some(value) = value.value().downcast_ref::<V>() {
                self.insert(key.clone(), value.clone());
                return Ok(());
            }
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::IndexSet,
            format!("cannot index hashmap with {index:?}"),
        ))
    }
}

impl<K> Object for HashMap<K, ValueRef>
where
    K: Object + std::hash::Hash + std::cmp::Eq + Clone,
{
    fn debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }

    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        if let Some(key) = index.downcast_ref::<K>() {
            if let Some(value) = self.get(key) {
                return Ok(value.clone());
            }

            // return Ok(Value::null());
            return Err(RuntimeError::key_not_found(index));
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::IndexGet,
            format!("cannot index hashmap with {index:?}"),
        ))
    }

    fn index_set(&mut self, index: &Value, value: ValueRef) -> Result<(), RuntimeError> {
        if let Some(key) = index.downcast_ref::<K>() {
            self.insert(key.clone(), value);
            return Ok(());
        }

        Err(RuntimeError::invalid_operation(
            super::OperateKind::IndexSet,
            format!("cannot index hashmap with {index:?}"),
        ))
    }

    fn make_iterator(
        &self,
    ) -> Result<Box<dyn Iterator<Item = ValueRef> + Send + Sync>, RuntimeError> {
        Ok(Box::new(self.clone().into_iter().map(|(k, v)| {
            ValueRef::new((ValueRef::new(k.clone()), v.clone()))
        })))
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
            "insert" => {
                if args.len() == 2 {
                    match args[0].value().downcast_ref::<K>() {
                        Some(key) => {
                            self.insert(key.clone(), args[1].clone());
                            return Ok(None);
                        }
                        None => {
                            return Err(RuntimeError::invalid_type::<K>(
                                "insert() argument 1 must be a key".to_string(),
                            ));
                        }
                    };
                }
                Err(RuntimeError::invalid_argument_count(1, args.len()))
            }
            "remove" => {
                if args.len() == 1 {
                    match args[0].value().downcast_ref::<K>() {
                        Some(key) => {
                            self.remove(key);
                            return Ok(None);
                        }
                        None => {
                            return Err(RuntimeError::invalid_type::<K>(
                                "insert() argument 1 must be a key".to_string(),
                            ));
                        }
                    };
                }

                Err(RuntimeError::invalid_argument_count(1, args.len()))
            }
            "keys" => {
                if args.is_empty() {
                    let keys = self.keys().cloned().collect::<Vec<_>>();
                    return Ok(Some(ValueRef::new(keys)));
                }

                Err(RuntimeError::invalid_argument_count(0, args.len()))
            }
            "values" => {
                if args.is_empty() {
                    let values = self.values().cloned().collect::<Vec<_>>();
                    return Ok(Some(ValueRef::new(values)));
                }

                Err(RuntimeError::invalid_argument_count(0, args.len()))
            }
            _ => Ok(None),
        }
    }
}
