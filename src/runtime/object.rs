mod array;
mod boolean;
mod emurator;
mod float;
mod function;
mod immd;
mod integer;
mod map;
mod null;
mod promise;
mod range;
mod string;
mod tuple;

pub use emurator::Enumerator;
pub use function::{Callable, NativeFunction, UserFunction};
pub use immd::Immd;
pub use null::Null;
pub use promise::Promise;
pub use range::Range;

use std::fmt;

use futures::Future;


use super::{RuntimeError, Value, ValueRef};

pub trait Object: std::any::Any + std::fmt::Debug {
    fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format!("{self:?}"))
    }

    /// arithmetic addition operation
    fn add(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Add,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    /// arithmetic subtraction operation
    fn sub(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Subtract,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    /// arithmetic multiplication operation
    fn mul(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Multiply,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    /// arithmetic division operation
    fn div(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Divide,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    /// arithmetic modulo operation
    fn modulo(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Modulo,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    /// arithmetic power operation
    fn pow(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Power,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    /// compare operation
    fn compare(&self, other: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Compare,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    /// logic and operation
    fn logic_and(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::LogicAnd,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    /// logic or operation
    fn logic_or(&self, other: &Value) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::LogicOr,
            format!("lhs: {self:?}, rhs: {other:?}"),
        ))
    }

    /// negate operation
    fn negate(&self) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Negate,
            format!("rhs: {self:?}"),
        ))
    }

    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Call,
            "unimplemented",
        ))
    }

    /// index get operation
    fn index_get(&self, index: &Value) -> Result<ValueRef, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::IndexGet,
            "unimplemented",
        ))
    }

    fn index_set(&mut self, index: &Value, value: ValueRef) -> Result<(), RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::IndexSet,
            "unimplemented",
        ))
    }

    fn property_get(&self, member: &str) -> Result<ValueRef, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::PropertyGet,
            "unimplemented",
        ))
    }

    fn property_set(&mut self, member: &str, value: ValueRef) -> Result<(), RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::PropertySet,
            "unimplemented",
        ))
    }

    fn property_call(
        &mut self,
        member: &str,
        args: &[ValueRef],
    ) -> Result<Option<Value>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::PropertyCall,
            format!("unimplemented {member} property call for {self:?}"),
        ))
    }

    fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::MakeIterator,
            "unimplemented",
        ))
    }

    fn iterator_has_next(&self) -> Result<bool, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::IteratorHasNext,
            "unimplemented",
        ))
    }

    fn iterate_next(&mut self) -> Result<ValueRef, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::IterateNext,
            "unimplemented",
        ))
    }

    fn make_slice(&self, range: ValueRef) -> Result<Value, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::MakeSlice,
            "unimplemented",
        ))
    }

    fn into_future(
        self: Box<Self>,
    ) -> Result<Box<dyn Future<Output = Value> + Unpin + Send + 'static>, RuntimeError> {
        Err(RuntimeError::invalid_operation(
            OperateKind::Await,
            "unimplemented",
        ))
    }
}



#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperateKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Compare,
    LogicAnd,
    LogicOr,
    Negate,
    Call,
    Range,
    IndexGet,
    IndexSet,
    PropertyGet,
    PropertySet,
    PropertyCall,
    MakeIterator,
    IteratorHasNext,
    IterateNext,
    MakeSlice,
    Display,
    TypeCast,
    Await,
}

impl fmt::Display for OperateKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OperateKind::Add => write!(f, "add"),
            OperateKind::Subtract => write!(f, "subtract"),
            OperateKind::Multiply => write!(f, "multiply"),
            OperateKind::Divide => write!(f, "divide"),
            OperateKind::Modulo => write!(f, "modulo"),
            OperateKind::Power => write!(f, "power"),
            OperateKind::Compare => write!(f, "compare"),
            OperateKind::LogicAnd => write!(f, "logic_and"),
            OperateKind::LogicOr => write!(f, "logic_or"),
            OperateKind::Negate => write!(f, "negate"),
            OperateKind::Call => write!(f, "call"),
            OperateKind::Range => write!(f, "range"),
            OperateKind::IndexGet => write!(f, "index_get"),
            OperateKind::IndexSet => write!(f, "index_set"),
            OperateKind::PropertyGet => write!(f, "property_get"),
            OperateKind::PropertySet => write!(f, "property_set"),
            OperateKind::PropertyCall => write!(f, "property_call"),
            OperateKind::MakeIterator => write!(f, "make_iterator"),
            OperateKind::IteratorHasNext => write!(f, "iterator_has_next"),
            OperateKind::IterateNext => write!(f, "iterate_next"),
            OperateKind::MakeSlice => write!(f, "make_slice"),
            OperateKind::Display => write!(f, "display"),
            OperateKind::TypeCast => write!(f, "type_cast"),
            OperateKind::Await => write!(f, "await"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::runtime::{ValueRef, Value};

    #[test]
    fn test_null() {
        let a = ValueRef::new(1_i64);
        let b = Value::new(2_i64);

        let c = a.borrow().add(&b);

        assert_eq!(c.unwrap(), 3);
    }
}
