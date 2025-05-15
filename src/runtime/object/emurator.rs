use std::fmt;

use crate::{Object, RuntimeError, ValueRef};

use super::metatable::MetaTable;

/// Enumerator
pub struct Enumerator {
    iter: Box<dyn Iterator<Item = ValueRef>>,
}

impl Enumerator {
    pub fn new(iter: Box<dyn Iterator<Item = ValueRef>>) -> Self {
        Self { iter }
    }

    pub fn next(&mut self) -> Option<ValueRef> {
        self.iter.next()
    }
}

impl fmt::Debug for Enumerator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Enumerator").finish()
    }
}

impl Object for Enumerator {
    fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Enumerator").finish()
    }

    fn iterate_next(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        Ok(self.next())
    }

    fn method_call(
        &mut self,
        method: &str,
        args: &[ValueRef],
    ) -> Result<Option<ValueRef>, RuntimeError> {
        ENUMERATOR_META_TABLE.method_call(self, method, args)
    }
}

static ENUMERATOR_META_TABLE: std::sync::LazyLock<MetaTable<Enumerator>> =
    std::sync::LazyLock::new(|| {
        MetaTable::new("Enumerator")
            .with_method("next", |this: &mut Enumerator, args| {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }
                Ok(this.next())
            })
            .with_method("enumerate", |this: &mut Enumerator, args| {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }

                let iter = std::mem::replace(&mut this.iter, Box::new(std::iter::empty()));

                let iter = iter
                    .enumerate()
                    .map(|(i, v)| ValueRef::new((ValueRef::new(i as i64), v)));

                Ok(Some(ValueRef::new(Enumerator::new(Box::new(iter)))))
            })
            .with_method("count", |this, args| {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }

                let iter = std::mem::replace(&mut this.iter, Box::new(std::iter::empty()));

                let count = iter.count();

                Ok(Some(ValueRef::new(count as i64)))
            })
    });
