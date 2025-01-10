#[macro_use]
extern crate rocket;

use lazy_static::lazy_static;
use rocket::request::{FromParam, Request};
use rocket::{response, serde::{json::Json, Deserialize, Serialize}, Build, Response, Rocket};
use rocket_okapi::okapi::{schemars, schemars::JsonSchema};
use rocket_okapi::{openapi, openapi_get_routes, swagger_ui::*};
use std::collections::HashMap;
use std::io::Cursor;
use rocket::http::ContentType;
use rocket::response::Responder;
use rocket_okapi::gen::OpenApiGenerator;
use rocket_okapi::okapi::openapi3::RefOr::Ref;
use rocket_okapi::okapi::openapi3::{RefOr, Responses};
use rocket_okapi::response::OpenApiResponderInner;

#[derive(FromForm, Serialize, Deserialize, JsonSchema, Debug)]
struct Filters {
    age: u8,
    active: bool,
}

struct NameGrade<'r> {
    name: &'r str,
    grade: u8,
}

#[derive(FromForm, Serialize, Deserialize, JsonSchema, Debug)]
struct User {
    uuid: String,
    name: String,
    age: u8,
    grade: u8,
    active: bool,
}

impl<'r> Responder<'r, 'r> for &'r User {
    fn respond_to(self, _: &'r Request<'_>) -> response::Result<'r> {
        let user = format!("Found user: {:?}", self);

        Response::build()
            .sized_body(user.len(), Cursor::new(user))
            .raw_header("X-USER-ID", self.uuid.to_string())
            .header(ContentType::Plain)
            .ok()
    }
}

impl<'r> OpenApiResponderInner for &'r User {
    fn responses(gen: &mut OpenApiGenerator) -> rocket_okapi::Result<Responses> {
        <String>::responses(gen)
    }
}

lazy_static! {
    static ref USERS: HashMap<&'static str, User> = {
        let mut map = HashMap::new();
        map.insert("3e3dd4ae-3c37-40c6-aa64-7061f284ce28", User {
            uuid: String::from("3e3dd4ae-3c37-40c6-aa64-7061f284ce28"),
            name: String::from("John Doe"),
            age: 18,
            grade: 1,
            active: true,
        });

        map
    };
}

#[openapi(tag = "Users")]
#[get("/user/<uuid>", rank = 1, format = "application/json")]
fn get_user(uuid: &str) -> Option<&User> {
    let user = USERS.get(uuid);

    match user {
        Some(u) => Some(u),
        None => None,
    }
}

// ======
#[launch]
fn rocket() -> Rocket<Build> {
    rocket::build()
        .mount("/", openapi_get_routes![get_user])
        .mount("/docs/", make_swagger_ui(&SwaggerUIConfig {
            url: "../openapi.json".to_owned(),
            ..Default::default()
        }), )
        .mount("/no-s/", routes![])
}
