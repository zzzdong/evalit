use parking_lot::{lock_api::MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::{fmt, sync::Arc};

use super::{Immd, Null, Object};
use crate::bytecode::{Constant, Primitive};

#[derive(Debug)]
pub struct Value(Box<dyn Object>);

impl Value {
    pub fn new<T: Object>(object: T) -> Self {
        Value(Box::new(object))
    }

    pub fn null() -> Self {
        Value(Box::new(Null))
    }

    pub fn downcast_ref<T: Object + 'static>(&self) -> Option<&T> {
        let any = &*self.0 as &dyn std::any::Any;
        any.downcast_ref::<T>()
    }

    pub fn downcast_mut<T: Object + 'static>(&mut self) -> Option<&mut T> {
        let any = &mut *self.0 as &mut dyn std::any::Any;
        any.downcast_mut::<T>()
    }

    pub fn as_object(&self) -> &dyn Object {
        &*self.0
    }

    pub fn as_object_mut(&mut self) -> &mut dyn Object {
        &mut *self.0
    }

    pub fn into_inner<T: Object + 'static>(self) -> Result<T, Box<dyn std::any::Any + 'static>> {
        (self.0 as Box<dyn std::any::Any>)
            .downcast::<T>()
            .map(|v| *v)
    }
}

#[derive(Debug, Clone)]
pub struct ValueRef(Arc<RwLock<Value>>);

impl ValueRef {
    pub fn new<T: Object + 'static>(object: T) -> Self {
        ValueRef(Arc::new(RwLock::new(Value::new(object))))
    }

    pub fn from_value(value: Value) -> Self {
        ValueRef(Arc::new(RwLock::new(value)))
    }

    pub fn value(&self) -> RwLockReadGuard<Value> {
        self.0.read()
    }

    pub fn value_mut(&self) -> RwLockWriteGuard<Value> {
        self.0.write()
    }

    pub fn as_object(&self) -> MappedRwLockReadGuard<'_, parking_lot::RawRwLock, dyn Object> {
        let guard = self.0.read();
        RwLockReadGuard::map(guard, |value: &Value| value.as_object())
    }

    pub fn as_object_mut(&self) -> MappedRwLockWriteGuard<'_, dyn Object> {
        let guard = self.0.write();
        RwLockWriteGuard::map(guard, |value: &mut Value| value.as_object_mut())
    }

    pub fn null() -> Self {
        ValueRef(Arc::new(RwLock::new(Value::null())))
    }

    pub fn immd(immd: isize) -> Self {
        Self::new(Immd(immd))
    }

    pub fn take(&self) -> Value {
        let mut lock = self.0.write();
        std::mem::replace(&mut *lock, Value::null())
    }

    pub fn from_constant(constant: &Constant) -> Self {
        match constant {
            Constant::String(s) => Self::new(s.to_string()),
        }
    }

    pub fn from_primitive(primitive: Primitive) -> Self {
        match primitive {
            Primitive::Null => Self::null(),
            Primitive::Byte(b) => Self::new(b),
            Primitive::Boolean(b) => Self::new(b),
            Primitive::Integer(i) => Self::new(i),
            Primitive::Float(f) => Self::new(f),
            Primitive::Char(c) => Self::new(c),
        }
    }
}

impl From<Value> for ValueRef {
    fn from(value: Value) -> Self {
        Self::from_value(value)
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value().as_object().debug(f)
    }
}
