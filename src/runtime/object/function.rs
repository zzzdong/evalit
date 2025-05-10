use std::{fmt, marker::PhantomData};

use crate::{bytecode::FunctionId, Object, RuntimeError, Value, ValueRef};

#[derive(Debug, Clone, Copy)]
pub struct UserFunction(FunctionId);

impl UserFunction {
    pub fn new(id: FunctionId) -> Self {
        Self(id)
    }

    pub fn id(&self) -> FunctionId {
        self.0
    }
}

impl Object for UserFunction {
    fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UserFunction({:?})", self.0)
    }
}

/// NativeFunction
pub struct NativeFunction {
    pub name: String,
    pub func: Box<dyn Function>,
}

impl NativeFunction {
    pub fn new(name: impl ToString, func: Box<dyn Function>) -> Self {
        Self {
            name: name.to_string(),
            func,
        }
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<NativeFunction`{}`>", self.name)
    }
}

impl Object for NativeFunction {
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        (self.func).call(args)
    }
}

/// Function trait for external functions.
pub trait Function: Send + 'static {
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError>;
}

pub struct IntoFunction<F, Args> {
    func: F,
    _marker: PhantomData<fn(Args) -> ()>,
}

impl<F, Args> Function for IntoFunction<F, Args>
where
    F: Callable<Args> + Clone + Send,
    Args: 'static,
{
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        self.func.call(args)
    }
}

pub trait IntoRet {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError>;
}

// impl IntoRet for Value {
//     fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
//         Ok(Some(self))
//     }
// }

impl<T: Object> IntoRet for T {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        Ok(Some(Value::new(self)))
    }
}

// impl IntoRet for Result<Value, RuntimeError> {
//     fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
//         self.map(Some)
//     }
// }

impl<T: Object> IntoRet for Result<T, RuntimeError> {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        self.map(|v| Some(Value::new(v)))
    }
}

impl<T: Object> IntoRet for Result<Option<T>, RuntimeError> {
    fn into_ret(self) -> Result<Option<Value>, RuntimeError> {
        self.map(|v| v.map(Value::new))
    }
}

pub trait FromValue: Sized {
    fn from_value(value: &ValueRef) -> Result<Self, RuntimeError>;
}


impl<T> FromValue for T
where
    T: Object + Clone,
{
    fn from_value(value: &ValueRef) -> Result<T, RuntimeError> {
        let value = value
            .downcast_ref::<T>()
            .ok_or(RuntimeError::invalid_type::<T>(value))?;

        Ok(value.clone())
    }
}

pub trait Callable<Args>: Clone + Sized + 'static {
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError>;

    fn into_function(self) -> IntoFunction<Self, Args> {
        IntoFunction {
            func: self,
            _marker: PhantomData,
        }
    }
}

impl<F, Ret> Callable<&[ValueRef]> for F
where
    F: Fn(&[ValueRef]) -> Ret + Clone + 'static,
    Ret: IntoRet,
{
    fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        (self)(args).into_ret()
    }
}

impl<F, Ret> Callable<()> for F
where
    F: Fn() -> Ret + Clone + 'static,
    Ret: IntoRet,
{
    fn call(&mut self, _args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
        self().into_ret()
    }
}

macro_rules! impl_callable {
    ($($idx: expr => $arg: ident),+) => {
        #[allow(non_snake_case)]
        impl<F, Ret, $($arg,)*> Callable<($($arg,)*)> for F
        where
            F: Fn($($arg,)*) -> Ret + Clone  + 'static,
            Ret: IntoRet,
            $( $arg: FromValue + 'static, )*
        {
            fn call(&mut self, args: &[ValueRef]) -> Result<Option<Value>, RuntimeError> {
                $(
                    let $arg = <$arg>::from_value(args.get($idx).ok_or(RuntimeError::invalid_argument::<$arg>($idx, "NoThing"))?)?;
                )*
                (self)($($arg,)*).into_ret()
            }
        }
    }
}

impl_callable!(0=>T0);
impl_callable!(0=>T0, 1=>T1);
impl_callable!(0=>T0, 1=>T1, 2=>T2);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11, 12=>T12);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11, 12=>T12, 13=>T13);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11, 12=>T12, 13=>T13, 14=>T14);
impl_callable!(0=>T0, 1=>T1, 2=>T2, 3=>T3, 4=>T4, 5=>T5, 6=>T6, 7=>T7, 8=>T8, 9=>T9, 10=>T10, 11=>T11, 12=>T12, 13=>T13, 14=>T14, 15=>T15);

/* use this when [feature(macro_metavar_expr)](https://github.com/rust-lang/rust/pull/122808) is available
macro_rules! impl_callable_tuple {
    ($($arg: ident),+) => {
        #[allow(non_snake_case)]
        impl<F, Ret, $($arg,)*> Callable<($($arg,)*)> for F
        where
            F: Fn($($arg,)*) -> Ret + Clone + Send + 'static,
            Ret: IntoRet,
            $( $arg: FromValue, )*
        {
            fn call(&mut self, args: &[Value]) -> Result<Option<Value>, RuntimeError> {
                $(
                    let $arg = <$arg>::from_value(&args[${index()}])?;
                )*
                (self)($($arg,)*).into_ret()
            }
        }
    }
}
impl_callable_tuple!(T0);
impl_callable_tuple!(T0, T1);
impl_callable_tuple!(T0, T1, T2);
impl_callable_tuple!(T0, T1, T2, T3);
impl_callable_tuple!(T0, T1, T2, T3, T4);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15);
impl_callable_tuple!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16);
 */

/* TODO:
pub struct Method<T> {
    pub name: String,
    pub func: Box<dyn MethodFunction<T>>,
}

impl<T> fmt::Debug for Method<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Method<{}.{}>", type_name::<T>(), self.Value)
    }
}

impl<T: Object> Method<T> {
    pub fn new<Args>(name: impl ToString, method: impl MethodCallable<T, Args>) -> Self
    where
        Args: 'static,
    {
        Method {
            name: name.to_string(),
            func: Box::new(method.into_function()),
        }
    }
}

/// Function trait for external functions.
pub trait MethodFunction<T>: Send + 'static {
    fn call(&mut self, this: &mut T, args: &[Value]) -> Result<Option<Value>, RuntimeError>;
}

pub struct IntoMethodFunction<F, Args> {
    func: F,
    _marker: PhantomData<fn(Args) -> ()>,
}

impl<T, F, Args> MethodFunction<T> for IntoMethodFunction<F, Args>
where
    F: MethodCallable<T, Args> + Clone,
    Args: 'static,
{
    fn call(&mut self, this: &mut T, args: &[Value]) -> Result<Option<Value>, RuntimeError> {
        self.func.call(this, args)
    }
}

pub trait MethodCallable<T, Args>: Clone + Send + Sized + 'static {
    fn call(&mut self, this: &mut T, args: &[Value]) -> Result<Option<Value>, RuntimeError>;

    fn into_function(self) -> IntoMethodFunction<Self, Args> {
        IntoMethodFunction {
            func: self,
            _marker: PhantomData,
        }
    }
}

impl<T, F, Ret> MethodCallable<T, &[Value]> for F
where
    T: Object,
    F: Fn(&mut T, &[Value]) -> Ret + Clone + Send + 'static,
    Ret: IntoRet,
{
    fn call(&mut self, this: &mut T, args: &[Value]) -> Result<Option<Value>, RuntimeError> {
        (self)(this, args).into_ret()
    }
}

impl<T, F, Ret> MethodCallable<T, ()> for F
where
    F: Fn(&T) -> Ret + Clone + Send + 'static,
    Ret: IntoRet,
{
    fn call(&mut self, this: &mut T, args: &[Value]) -> Result<Option<Value>, RuntimeError> {
        self(this).into_ret()
    }
}

impl<T, F, Ret, Arg> MethodCallable<T, Arg> for F
where
    F: Fn(&mut T, Arg) -> Ret + Clone + Send + 'static,
    Ret: IntoRet,
    Arg: FromValue,
{
    fn call(&mut self, this: &mut T, args: &[Value]) -> Result<Option<Value>, RuntimeError> {
        let arg = Arg::from_value(&args[0])?;

        (self)(this, arg).into_ret()
    }
}

*/
