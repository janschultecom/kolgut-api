
extern crate hyper;
extern crate libc;

use std::ffi::CStr;
use std::str;
use libc::c_char;


use hyper::{Body, Response, Server};
use hyper::rt::Future;
use hyper::service::service_fn_ok;

static TEXT: &str = "Hello, World!";

#[no_mangle]
    pub extern "C" fn serve(value: *const c_char) {
    let c_value = unsafe { CStr::from_ptr(value).to_bytes() };
    let body = match str::from_utf8(c_value) {
        Ok(v) => v,
        Err(_) => "Nope",
    };

    let addr = ([127, 0, 0, 1], 3000).into();

    let new_svc = move || {
        let x = body.clone();
        service_fn_ok(move |_req|{
            Response::new(Body::from(x))
        })
    };

    let server = Server::bind(&addr)
        .serve(new_svc)
        .map_err(|e| eprintln!("server error: {}", e));

    hyper::rt::run(server);
}

