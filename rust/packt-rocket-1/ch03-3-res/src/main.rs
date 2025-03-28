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

// ==== for swagger example =====
// |
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
// |
// ==== for swagger example =====


// ==== for custom response =====
// |
impl<'r> Responder<'r, 'r> for &'r User {
    fn respond_to(self, req: &'r Request<'_>) -> response::Result<'r> {
        // let user = format!("Found user: {:?}", self);
        // let user = Json(self);

        Response::
            // build()
            // .sized_body(user.len(), Cursor::new(user))
            build_from(Json(self).respond_to(req)?)
            .raw_header("X-USER-ID", self.uuid.to_string())
            // .header(ContentType::Plain)
            .header(ContentType::JSON)
            .ok()
    }
}

impl<'r> OpenApiResponderInner for &'r User {
    fn responses(gen: &mut OpenApiGenerator) -> rocket_okapi::Result<Responses> {
        // <String>::responses(gen)
        <Json<&User>>::responses(gen)
    }
}
// |
// ==== for custom response =====

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
#[get("/user/<uuid>", rank = 1)]
// fn get_user(uuid: &str) -> Option<&User> {
fn get_user(uuid: Uuid) -> Option<&'static User> {
    let user = USERS.get(uuid.0.as_str());

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
