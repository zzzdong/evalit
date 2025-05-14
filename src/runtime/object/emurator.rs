use std::{fmt};

use crate::{Object, RuntimeError, Value, ValueRef};

use super::metatable::MetaTable;

/// Enumerator
pub struct Enumerator {
    iter: Box<dyn Iterator<Item = ValueRef>>,
    next: Option<ValueRef>,
}

impl Enumerator {
    pub fn new(iter: Box<dyn Iterator<Item = ValueRef>>) -> Self {
        let mut iter = iter;
        let next = iter.next();
        Self { iter, next }
    }

    pub fn next(&mut self) -> Option<ValueRef> {
        match self.next {
            Some(_) => std::mem::replace(&mut self.next, self.iter.next()),
            None => None,
        }
    }
}

impl fmt::Debug for Enumerator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Enumerator").finish()
    }
}

// impl Iterator for Enumerator {
//     type Item = ValueRef;

//     fn next(&mut self) -> Option<Self::Item> {
//         self.next.take().or_else(|| self.iter.next())
//     }
// }

impl Object for Enumerator {
    fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Enumerator").finish()
    }

    fn iterate_next(&mut self) -> Result<ValueRef, RuntimeError> {
        let next = self.next();
        Ok(next.expect("iterator exhausted"))
    }

    fn iterator_has_next(&self) -> Result<bool, RuntimeError> {
        Ok(self.next.is_some())
    }
}

static ENUMERATOR_META_TABLE: std::sync::LazyLock<MetaTable<Enumerator>> =
    std::sync::LazyLock::new(|| {
        MetaTable::new("Enumerator")
            .with_method("next", |this: &mut Enumerator, args| {
                if !args.is_empty() {
                    return Err(RuntimeError::invalid_argument_count(0, args.len()));
                }
                Ok(this.next().map(|v| v.take()))
            })
    });
