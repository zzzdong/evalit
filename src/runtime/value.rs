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

    pub(crate) fn from_oject(object: Box<dyn Object>) -> Self {
        ValueRef(Rc::new(RefCell::new(Value(object))))
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

    pub fn from_constant(constant: Constant) -> Self {
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

    // pub fn downcast_ref<'a, T: Object + 'static>(&'a self) -> Option<&'a T> {
    //     let obj = self.0.borrow();
    //     let any = (&*obj as &dyn std::any::Any);
    //     any.downcast_ref::<T>()
    // }

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

// impl<T: Object> From<T> for Value {
//     fn from(value: T) -> Self {
//         Value::new(value)
//     }
// }

impl fmt::Display for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.borrow().debug(f)
    }
}

// impl<T: Object> PartialEq<T> for ValueRef {
//     fn eq(&self, other: &T) -> bool {
//         match self.borrow().compare(&Value::new(other)) {
//             Ok(ordering) => ordering == std::cmp::Ordering::Equal,
//             Err(_) => false,
//         }
//     }
// }

// impl Object for ValueRef {
//     fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         self.borrow().debug(f)
//     }

//     fn add(&self, other: &ValueRef) -> Result<ValueRef, RuntimeError> {
//         self.borrow().add(other)
//     }

//     fn sub(&self, other: &ValueRef) -> Result<ValueRef, RuntimeError> {
//         self.borrow().sub(other)
//     }

//     fn mul(&self, other: &ValueRef) -> Result<ValueRef, RuntimeError> {
//         self.borrow().mul(other)
//     }

//     fn div(&self, other: &ValueRef) -> Result<ValueRef, RuntimeError> {
//         self.borrow().div(other)
//     }

//     fn modulo(&self, other: &ValueRef) -> Result<ValueRef, RuntimeError> {
//         self.borrow().modulo(other)
//     }

//     fn pow(&self, other: &ValueRef) -> Result<ValueRef, RuntimeError> {
//         self.borrow().pow(other)
//     }

//     fn compare(&self, other: &ValueRef) -> Result<std::cmp::Ordering, RuntimeError> {
//         self.borrow().compare(other)
//     }

//     fn logic_and(&self, other: &ValueRef) -> Result<ValueRef, RuntimeError> {
//         self.borrow().logic_and(other)
//     }

//     fn logic_or(&self, other: &ValueRef) -> Result<ValueRef, RuntimeError> {
//         self.borrow().logic_or(other)
//     }

//     fn negate(&self) -> Result<ValueRef, RuntimeError> {
//         self.borrow().negate()
//     }

//     fn call(&mut self, args: &[ValueRef]) -> Result<Option<ValueRef>, RuntimeError> {
//         self.borrow_mut().call(args)
//     }

//     fn index_get(&self, index: &ValueRef) -> Result<ValueRef, RuntimeError> {
//         self.borrow().index_get(index)
//     }

//     fn index_set(&mut self, index: &ValueRef, value: ValueRef) -> Result<(), RuntimeError> {
//         self.borrow_mut().index_set(index, value)
//     }

//     fn property_get(&mut self, member: &str) -> Result<ValueRef, RuntimeError> {
//         self.borrow_mut().property_get(member)
//     }

//     fn property_set(&mut self, member: &str, value: ValueRef) -> Result<(), RuntimeError> {
//         self.borrow_mut().property_set(member, value)
//     }

//     fn property_call(
//         &mut self,
//         member: &str,
//         args: &[ValueRef],
//     ) -> Result<Option<ValueRef>, RuntimeError> {
//         self.borrow_mut().property_call(member, args)
//     }

//     fn make_iterator(&self) -> Result<Box<dyn Iterator<Item = ValueRef>>, RuntimeError> {
//         self.borrow().make_iterator()
//     }

//     fn iterator_has_next(&self) -> Result<bool, RuntimeError> {
//         self.borrow().iterator_has_next()
//     }

//     fn iterate_next(&mut self) -> Result<ValueRef, RuntimeError> {
//         self.borrow_mut().iterate_next()
//     }

//     fn make_slice(&self, range: &ValueRef) -> Result<ValueRef, RuntimeError> {
//         self.borrow().make_slice(range)
//     }

//     fn into_future(
//         self: Box<Self>,
//     ) -> Result<Box<dyn Future<Output = ValueRef> + Unpin + Send + 'static>, RuntimeError> {
//         // 这里需要特殊处理，因为我们需要获取内部的Box<dyn Object>
//         match Rc::try_unwrap(self.0) {
//             Ok(refcell) => match refcell.into_inner() {
//                 inner => inner.0.into_future(),
//             },
//             Err(_) => Err(RuntimeError::invalid_operation(
//                 OperateKind::Await,
//                 "Cannot convert shared value into future",
//             )),
//         }
//     }
// }
