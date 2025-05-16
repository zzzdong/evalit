use std::{
    cell::{Ref, RefCell, RefMut},
    fmt,
    rc::Rc,
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

    pub fn object(&self) -> &dyn Object {
        &*self.0
    }

    pub fn into_inner<T: Object + 'static>(self) -> Result<T, Box<dyn std::any::Any + 'static>> {
        (self.0 as Box<dyn std::any::Any>)
            .downcast::<T>()
            .map(|v| *v)
    }
}

#[derive(Debug, Clone)]
pub struct ValueRef(Rc<RefCell<Value>>);

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

impl From<Value> for ValueRef {
    fn from(value: Value) -> Self {
        ValueRef(Rc::new(RefCell::new(value)))
    }
}

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.borrow().debug(f)
    }
}
