#[cfg(feature = "async")]
#[tokio::main]
async fn main() {
    scripting::run().await
}

#[cfg(feature = "async")]
mod scripting {

    use std::{fmt, sync::Arc};

    use evalit::{Environment, Module, Object, RuntimeError, VM, ValueRef, compile};
    use hyper::{
        body::Incoming,
        header::{HeaderName, HeaderValue},
    };
    use hyper_util::rt::TokioExecutor;
    use tokio::net::TcpListener;

    pub async fn run() {
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
                    service_fn(|req| {
                        let script_cloned = script.clone();
                        let remote_addr_cloned = remote_addr.clone();

                        async move {
                            let mut env = Environment::new();

                            env.insert("request", RequestWrapper::new(req, remote_addr_cloned));
                            env.insert("response", ResponseWrapper::new());

                            let mut vm = VM::new(script_cloned, env.clone());

                            let result = vm.run().await;
                            assert!(result.is_ok());

                            let req = env.remove_as::<RequestWrapper>("request").unwrap();
                            let rsp = env.remove_as::<ResponseWrapper>("response").unwrap();

                            println!(
                                "Accepted request from: {:?}",
                                req.inner.headers().get("X-Real-Ip")
                            );

                            Ok::<_, String>(rsp.into_inner())
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

            $response.set_header("Content-Type", "application/json");
            $response.set_body("{\"message\": \"Hello, World!\"}");
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

        fn call_method(
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
                            if let Some(header_value) =
                                self.inner.headers().get(header_name.as_str())
                            {
                                return Ok(header_value
                                    .to_str()
                                    .ok()
                                    .map(|header_value| ValueRef::new(header_value.to_string())));
                            }

                            Ok(None)
                        }
                        None => {
                            Err(evalit::RuntimeError::invalid_argument::<String>(
                                0, &args[0],
                            ))
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

                            Ok(None)
                        }
                        _ => {
                            Err(evalit::RuntimeError::invalid_argument::<String>(
                                0, &args[0],
                            ))
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

                            Ok(None)
                        }
                        None => {
                            Err(evalit::RuntimeError::invalid_argument::<String>(
                                0, &args[0],
                            ))
                        }
                    }
                }

                "remote_addr" => {
                    if !args.is_empty() {
                        return Err(evalit::RuntimeError::invalid_argument_count(1, 0));
                    }
                    Ok(Some(ValueRef::new(self.remote_addr.clone())))
                }

                _ => {
                    Err(evalit::RuntimeError::missing_method::<Self>(method))
                }
            }
        }
    }

    struct ResponseWrapper {
        inner: hyper::Response<String>,
    }

    impl ResponseWrapper {
        fn new() -> Self {
            Self {
                inner: hyper::Response::new(String::new()),
            }
        }

        fn into_inner(self) -> hyper::Response<String> {
            self.inner
        }

        // 新增: 设置响应状态码
        fn set_status(&mut self, status: u16) {
            *self.inner.status_mut() = hyper::StatusCode::from_u16(status).unwrap();
        }

        // 新增: 设置响应头部
        fn set_header(&mut self, header_name: &str, header_value: &str) {
            self.inner.headers_mut().insert(
                hyper::header::HeaderName::from_bytes(header_name.as_bytes()).unwrap(),
                hyper::header::HeaderValue::from_bytes(header_value.as_bytes()).unwrap(),
            );
        }

        // 新增: 设置响应主体
        fn set_body(&mut self, body: String) {
            *self.inner.body_mut() = body;
        }
    }

    impl fmt::Debug for ResponseWrapper {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Response")
                .field("inner", &self.inner)
                .finish()
        }
    }

    impl Object for ResponseWrapper {
        fn call_method(
            &mut self,
            method: &str,
            args: &[ValueRef],
        ) -> Result<Option<ValueRef>, RuntimeError> {
            match method {
                "set_status" => {
                    if args.len() != 1 {
                        return Err(RuntimeError::invalid_argument_count(1, args.len()));
                    }

                    if let Some(status) = args[0].value().downcast_ref::<u16>() {
                        self.set_status(*status);
                        return Ok(None);
                    }

                    Err(RuntimeError::invalid_argument::<u16>(0, &args[0]))
                }

                "set_header" => {
                    if args.len() != 2 {
                        return Err(RuntimeError::invalid_argument_count(2, args.len()));
                    }

                    if let (Some(header_name), Some(header_value)) = (
                        args[0].value().downcast_ref::<String>(),
                        args[1].value().downcast_ref::<String>(),
                    ) {
                        self.set_header(header_name, header_value);
                        return Ok(None);
                    }

                    Err(RuntimeError::invalid_argument::<String>(0, &args[0]))
                }

                "set_body" => {
                    if args.len() != 1 {
                        return Err(RuntimeError::invalid_argument_count(1, args.len()));
                    }

                    if let Some(body) = args[0].value().downcast_ref::<String>() {
                        self.set_body(body.clone());
                        return Ok(None);
                    }

                    Err(RuntimeError::invalid_argument::<String>(0, &args[0]))
                }

                _ => Err(RuntimeError::missing_method::<Self>(method)),
            }
        }
    }
}

#[cfg(not(feature = "async"))]
fn main() {
    println!("This example requires the 'async' feature");
}
