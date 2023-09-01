use std::fmt;

use crate::Object;

#[derive(Debug, Clone, Copy)]
pub struct Immd(pub isize);

impl Object for Immd {
    fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Immd({})", self.0)
    }
}
