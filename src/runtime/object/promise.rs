use std::{fmt, pin::Pin};

use futures::FutureExt;

use crate::{Object, ValueRef};

/// Promise
pub struct Promise(pub(crate) Pin<Box<dyn Future<Output = ValueRef> + 'static>>);

impl Promise {
    pub fn new(fut: impl Future<Output = ValueRef> + 'static) -> Self {
        Self(Box::pin(fut))
    }
}

impl Future for Promise {
    type Output = ValueRef;

    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        self.0.poll_unpin(cx)
    }
}

impl fmt::Debug for Promise {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Promise").finish()
    }
}

impl Object for Promise {}
