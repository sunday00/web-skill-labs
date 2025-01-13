#[macro_use]
extern crate rocket;

use lazy_static::lazy_static;
use rocket::http::ContentType;
use rocket::response::status::NotFound;
use rocket::response::Responder;
use rocket::serde::json::Json;
use rocket::serde::{Deserialize, Serialize};
use rocket::{http::Status, request::FromParam, response, response::status, Request, Response};
use rocket::{Build, Rocket};
use rocket_okapi::gen::OpenApiGenerator;
use rocket_okapi::okapi::openapi3::Responses;
use rocket_okapi::okapi::{schemars, schemars::JsonSchema};
use rocket_okapi::response::OpenApiResponderInner;
use rocket_okapi::swagger_ui::{make_swagger_ui, SwaggerUIConfig};
use rocket_okapi::{openapi, openapi_get_routes};
use std::collections::HashMap;
use std::vec::Vec;

#[derive(Serialize, Deserialize, FromForm, JsonSchema, Debug)]
struct Filters {
    age: u8,
    active: bool,
}

#[derive(Serialize, Deserialize, FromForm, JsonSchema, Debug)]
#[schemars(example = "example_uuid")]
struct Uuid(String);

fn example_uuid() -> String {
    String::from("3e3dd4ae-3c37-40c6-aa64-7061f284ce28")
}

impl<'r> FromParam<'r> for Uuid {
    type Error = ();

    fn from_param(param: &'r str) -> Result<Self, Self::Error> {
        Ok(Uuid(String::from(param)))
    }
}

#[derive(Serialize, Deserialize, JsonSchema, Debug)]
struct User {
    uuid: Uuid,
    name: String,
    age: u8,
    grade: u8,
    active: bool,
}

impl<'r> Responder<'r, 'r> for &'r User {
    fn respond_to(self, req: &'r Request<'_>) -> response::Result<'r> {
        Response::
        build_from(Json(self).respond_to(req)?)
            .raw_header("X-USER-ID", self.uuid.0.to_string())
            .header(ContentType::JSON)
            .ok()
    }
}

impl<'r> OpenApiResponderInner for &'r User {
    fn responses(gen: &mut OpenApiGenerator) -> rocket_okapi::Result<Responses> {
        <Json<&User>>::responses(gen)
    }
}

lazy_static! {
    static ref USERS: HashMap<&'static str, User> = {
        let mut map = HashMap::new();
        map.insert(
            "3e3dd4ae-3c37-40c6-aa64-7061f284ce28",
            User {
                // uuid: String::from("3e3dd4ae-3c37-40c6-aa64-7061f284ce28"),
                uuid: Uuid(String::from("3e3dd4ae-3c37-40c6-aa64-7061f284ce28")),
                name: String::from("John Doe"),
                age: 18,
                grade: 1,
                active: true,
            },
        );
        map
    };
}

#[openapi(tag = "Users")]
#[get("/user/<uuid>", rank = 1)]
// fn user(uuid: Uuid) -> Result<&'static User, NotFound<&'static str>> {
// fn user(uuid: Uuid) -> Option<&'static User> {
fn user(uuid: Uuid) -> Result<&'static User, Status> {
    let user = USERS.get(uuid.0.as_str());

    match user {
        Some(user) => Ok(user),
        None => Err(Status::Forbidden),
    }
}

#[launch]
fn rocket() -> Rocket<Build> {
    rocket::build().mount("/", openapi_get_routes![user])
        .mount("/docs/", make_swagger_ui(&SwaggerUIConfig {
            url: "../openapi.json".to_owned(),
            ..Default::default()
        }), )
        .mount("/no-s/", routes![])
        .register("/", catchers![not_found, forbidden])
}

#[catch(404)]
fn not_found(req: &Request) -> String {
    format!("We cannot find this page {}.", req.uri())
}

#[catch(403)]
fn forbidden(req: &Request) -> String {
    format!("Access forbidden {}.", req.uri())
}