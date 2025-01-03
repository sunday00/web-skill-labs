#[macro_use]
extern crate rocket;

use rocket::http::uri::Origin;
use rocket::http::Status;
use rocket::response::Redirect;
use rocket::serde::json::{json, Value};
use rocket::serde::{json::Json, Serialize};

const API: Origin<'static> = uri!("/api");

#[get("/")]
fn index() -> &'static str {
    "hello rocket"
}

#[get("/google-keep-desktop/<_platform>/<current_version>?<msg>")]
fn google_keep_desktop(_platform: &str, current_version: &str, msg: Option<&str>) -> Result<Value, Status> {
    if let Some(_msg) = msg {
        // TODO
        // return Status::Ok;
        return Err(Status::BadRequest);
    }


    // Status::Ok
    Ok(json!({"msg": current_version}))
}

#[get("/go")]
fn redirect_to_home() -> Redirect {
    // Redirect::to(uri!("/gg/google-keep-desktop/mac/v1"))
    Redirect::to(uri!(API, google_keep_desktop("mac", "v1", None as Option<&str>)))
}

#[get("/j-e")]
fn json_example() -> Value {
    json!({ "status": "ok"})
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index, redirect_to_home])
        .mount(API, routes![google_keep_desktop, json_example])
}