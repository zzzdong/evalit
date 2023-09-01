use std::fmt;

use crate::{Object, RuntimeError, ValueRef};

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

    fn iterate_next(&mut self) -> Result<ValueRef, RuntimeError> {
        let old = self.next.take();
        self.next = self.iter.next();
        Ok(old.expect("iterator exhausted"))
    }

    fn iterator_has_next(&self) -> Result<bool, RuntimeError> {
        Ok(self.next.is_some())
    }
}
