use std::{
    cell::{Ref, RefCell, RefMut},
    fmt,
    rc::Rc,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use crate::bytecode::{Constant, Primitive};

use super::{Immd, Null, Object};

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

// 同步版本
#[cfg(not(feature = "async"))]
#[derive(Debug, Clone)]
pub struct ValueRef(Rc<RefCell<Value>>);

#[cfg(not(feature = "async"))]
impl ValueRef {
    pub fn new<T: Object>(object: T) -> Self {
        ValueRef(Rc::new(RefCell::new(Value::new(object))))
    }

    pub fn from_value(value: Value) -> Self {
        ValueRef(Rc::new(RefCell::new(value)))
    }

    pub fn value(&self) -> Ref<Value> {
        self.0.borrow()
    }

    pub fn take(self) -> Value {
        self.0.replace(Value::null())
    }

    pub fn borrow(&self) -> Ref<dyn Object> {
        Ref::map(self.0.borrow(), |obj| obj.0.as_ref() as &dyn Object)
    }

    pub fn borrow_mut(&mut self) -> RefMut<dyn Object> {
        RefMut::map(self.0.borrow_mut(), |obj| obj.0.as_mut() as &mut dyn Object)
    }

    pub fn null() -> Self {
        ValueRef(Rc::new(RefCell::new(Value::new(Null))))
    }

    pub fn immd(immd: isize) -> Self {
        ValueRef::new(Immd(immd))
    }

    pub fn from_constant(constant: &Constant) -> Self {
        match constant {
            Constant::String(s) => ValueRef::new(s.as_str().to_string()),
        }
    }

    pub fn from_primitive(primitive: Primitive) -> Self {
        match primitive {
            Primitive::Null => ValueRef::null(),
            Primitive::Byte(b) => ValueRef::new(b),
            Primitive::Boolean(b) => ValueRef::new(b),
            Primitive::Integer(i) => ValueRef::new(i),
            Primitive::Float(f) => ValueRef::new(f),
            Primitive::Char(c) => ValueRef::new(c),
        }
    }

    pub fn downcast_ref<T: Object + 'static>(&self) -> Option<Ref<'_, T>> {
        Ref::filter_map(self.0.borrow(), |obj| {
            ((obj.0).as_ref() as &dyn std::any::Any).downcast_ref::<T>()
        })
        .ok()
    }

    pub fn downcast_mut<T: Object + 'static>(&mut self) -> Option<RefMut<'_, T>> {
        RefMut::filter_map(self.0.borrow_mut(), |obj| {
            ((obj.0).as_mut() as &mut dyn std::any::Any).downcast_mut::<T>()
        })
        .ok()
    }
}

#[cfg(not(feature = "async"))]
impl From<Value> for ValueRef {
    fn from(value: Value) -> Self {
        ValueRef(Rc::new(RefCell::new(value)))
    }
}

// 异步版本
#[cfg(feature = "async")]
#[derive(Debug, Clone)]
pub struct ValueRef(Arc<RwLock<Value>>);

#[cfg(feature = "async")]
impl ValueRef {
    pub fn new<T: Object + 'static>(object: T) -> Self {
        ValueRef(Arc::new(RwLock::new(Value::new(object))))
    }

    pub fn from_value(value: Value) -> Self {
        ValueRef(Arc::new(RwLock::new(value)))
    }

    pub fn value(&self) -> RwLockReadGuard<Value> {
        self.0.read().unwrap()
    }

    pub fn value_mut(&self) -> RwLockWriteGuard<Value> {
        self.0.write().unwrap()
    }

    pub fn null() -> Self {
        ValueRef(Arc::new(RwLock::new(Value::null())))
    }

    pub fn immd(immd: isize) -> Self {
        Self::new(Immd(immd))
    }

    pub fn take(&self) -> Value {
        let mut lock = self.0.write().unwrap();
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

#[cfg(feature = "async")]
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
