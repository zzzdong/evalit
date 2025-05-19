use std::{fmt, sync::Arc};

use evalit::{Environment, Module, Object, VM, ValueRef, compile};
use hyper::{
    body::Incoming,
    header::{HeaderName, HeaderValue},
};
use hyper_util::rt::TokioExecutor;
use tokio::net::TcpListener;

#[tokio::main]
async fn main() {
    use hyper::service::service_fn;
    use hyper_util::rt::TokioIo;

    let listener = TcpListener::bind("127.0.0.1:5000").await.unwrap();

    println!("Listening on http://{}", listener.local_addr().unwrap());

    let script = script();

    while let Ok((socket, remote_addr)) = listener.accept().await {
        let server = hyper_util::server::conn::auto::Builder::new(TokioExecutor::new());

        let script = script.clone();
        let remote_addr = remote_addr.to_string();

        tokio::task::spawn(async move {
            let ret = server.serve_connection_with_upgrades(
                TokioIo::new(socket),
                service_fn(|mut req| {
                    let script_cloned = script.clone();
                    let remote_addr_cloned = remote_addr.clone();

                    async move {
                        let mut env = Environment::new();

                        env.insert("request", RequestWrapper::new(req, remote_addr_cloned));

                        let mut vm = VM::new(script_cloned, env.clone());

                        let result = vm.run().await;
                        assert!(result.is_ok());

                        let req = env.remove_as::<RequestWrapper>("request").unwrap();

                        println!("{:?}", req.inner.headers().get("X-Real-Ip"));

                        Ok::<_, String>(hyper::Response::new("Hello World!".to_string()))
                    }
                }),
            );

            if let Err(e) = ret.await {
                log::error!("serve_connection error: {:?}", e);
            }
        });
    }
}

fn script() -> Arc<Module> {
    let mut env = Environment::new();
    env.insert("request", ());

    let script = r#"

            request.set_header("X-Hello", "World");
            request.remove_header("X-Hello");
            let remote_addr = request.remote_addr();
            request.set_header("X-Real-Ip", remote_addr);
        
    "#;

    compile(script, &env).unwrap()
}

struct RequestWrapper {
    inner: hyper::Request<Incoming>,
    remote_addr: String,
}

impl RequestWrapper {
    fn new(inner: hyper::Request<Incoming>, remote_addr: String) -> Self {
        Self { inner, remote_addr }
    }
}

impl fmt::Debug for RequestWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Request")
            .field("inner", &self.inner)
            .finish()
    }
}
impl Object for RequestWrapper {
    fn debug(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }

    fn method_call(
        &mut self,
        method: &str,
        args: &[evalit::ValueRef],
    ) -> Result<Option<evalit::ValueRef>, evalit::RuntimeError> {
        match method {
            "get_header" => {
                if args.len() != 1 {
                    return Err(evalit::RuntimeError::invalid_argument_count(1, 0));
                }

                match args[0].value().downcast_ref::<String>() {
                    Some(header_name) => {
                        if let Some(header_value) = self.inner.headers().get(header_name.as_str()) {
                            return Ok(header_value
                                .to_str()
                                .ok()
                                .map(|header_value| ValueRef::new(header_value.to_string())));
                        }

                        return Ok(None);
                    }
                    None => {
                        return Err(evalit::RuntimeError::invalid_argument::<String>(
                            0, &args[0],
                        ));
                    }
                }
            }

            "set_header" => {
                if args.len() != 2 {
                    return Err(evalit::RuntimeError::invalid_argument_count(2, 0));
                }

                match (
                    args[0].value().downcast_ref::<String>(),
                    args[1].value().downcast_ref::<String>(),
                ) {
                    (Some(header_name), Some(header_value)) => {
                        self.inner.headers_mut().insert(
                            HeaderName::from_bytes(header_name.as_str().as_bytes()).unwrap(),
                            HeaderValue::from_bytes(header_value.as_str().as_bytes()).unwrap(),
                        );

                        return Ok(None);
                    }
                    _ => {
                        return Err(evalit::RuntimeError::invalid_argument::<String>(
                            0, &args[0],
                        ));
                    }
                }
            }

            "remove_header" => {
                if args.len() != 1 {
                    return Err(evalit::RuntimeError::invalid_argument_count(1, 0));
                }

                match args[0].value().downcast_ref::<String>() {
                    Some(header_name) => {
                        self.inner.headers_mut().remove(header_name.as_str());

                        return Ok(None);
                    }
                    None => {
                        return Err(evalit::RuntimeError::invalid_argument::<String>(
                            0, &args[0],
                        ));
                    }
                }
            }

            "remote_addr" => {
                if !args.is_empty() {
                    return Err(evalit::RuntimeError::invalid_argument_count(1, 0));
                }
                return Ok(Some(ValueRef::new(self.remote_addr.clone())));
            }

            _ => {
                return Err(evalit::RuntimeError::missing_method::<Self>(method));
            }
        }
    }
}
