#[macro_use]
extern crate rocket;

use rocket::http::ContentType;
use rocket::response::Responder;
use rocket::serde::json::Json;
use rocket::serde::{Deserialize, Serialize};
use rocket::{request::FromParam, response, Request, Response, State};
use rocket::{Build, Rocket};
use rocket_okapi::gen::OpenApiGenerator;
use rocket_okapi::okapi::openapi3::Responses;
use rocket_okapi::okapi::{schemars, schemars::JsonSchema};
use rocket_okapi::response::OpenApiResponderInner;
use rocket_okapi::swagger_ui::{make_swagger_ui, SwaggerUIConfig};
use rocket_okapi::{openapi, openapi_get_routes};
use std::sync::atomic::{AtomicU64, Ordering};
use std::vec::Vec;
use sqlx::FromRow;
use uuid::Uuid;

#[derive(Deserialize)]
struct Config {
    database_url: String,
}

#[derive(Serialize, Deserialize, FromForm, JsonSchema, Debug)]
struct Filters {
    age: u8,
    active: bool,
}

#[derive(Serialize, Deserialize, FromForm, JsonSchema, Debug)]
#[schemars(example = "example_uuid")]
struct UuidReq(String);

fn example_uuid() -> String {
    String::from("3e3dd4ae-3c37-40c6-aa64-7061f284ce28")
}

impl<'r> FromParam<'r> for UuidReq {
    type Error = ();

    fn from_param(param: &'r str) -> Result<Self, Self::Error> {
        Ok(UuidReq(String::from(param)))
    }
}

#[derive(Serialize, Deserialize, JsonSchema, Debug, FromRow)]
#[sqlx(rename_all = "camelCase")]
struct User {
    uuid: Uuid,
    name: String,
    age: u8,
    grade: u8,
    // active: bool,
    #[sqlx(rename="active")]
    present: bool,
    #[sqlx(default)]
    not_in_database: String
}

impl<'r> Responder<'r, 'r> for &'r User {
    fn respond_to(self, req: &'r Request<'_>) -> response::Result<'r> {
        Response::
        build_from(Json(self).respond_to(req)?)
            .raw_header("X-USER-ID", self.uuid.to_string())
            .header(ContentType::JSON)
            .ok()
    }
}

impl<'r> OpenApiResponderInner for &'r User {
    fn responses(gen: &mut OpenApiGenerator) -> rocket_okapi::Result<Responses> {
        <Json<&User>>::responses(gen)
    }
}

#[derive(Serialize, JsonSchema, Debug)]
struct Users<'a>(Vec<&'a User>);

impl<'r> Responder<'r, 'r> for Users<'r> {
    fn respond_to(self, req: &'r Request<'_>) -> response::Result<'r> {
        Response::build_from(Json(self).respond_to(req)?)
            .header(ContentType::JSON)
            .ok()
    }
}

impl<'r> OpenApiResponderInner for Users<'r> {
    fn responses(gen: &mut OpenApiGenerator) -> rocket_okapi::Result<Responses> {
        <Json<&Users>>::responses(gen)
    }
}

struct VisitorCounter {
    visitor: AtomicU64,
}

impl VisitorCounter {
    fn increment_counter(&self) {
        self.visitor.fetch_add(1, Ordering::Relaxed);
        println!("the number of visitor counter is : {}", self.visitor.load(Ordering::Relaxed));
    }
}

#[openapi(tag = "Users")]
#[get("/user/<uuid>", rank = 1)]
fn user(counter: &State<VisitorCounter>, uuid: UuidReq) -> Option<&User> {
    // counter.visitor.fetch_add(1, Ordering::Relaxed);
    counter.increment_counter();
    println!("visitors: {}", counter.visitor.load(Ordering::Relaxed));

    USERS.get(uuid.0.as_str())
}

#[openapi(tag = "Users")]
#[get("/user", rank = 1)]
fn users(counter: &State<VisitorCounter>) -> Users {
    // counter.visitor.fetch_add(1, Ordering::Relaxed);
    counter.increment_counter();
    println!("visitors: {}", counter.visitor.load(Ordering::Relaxed));

    Users(USERS.values().collect())
}

#[launch]
fn rocket() -> Rocket<Build> {
    let visitor_counter = VisitorCounter {
        visitor: AtomicU64::new(0),
    };

    rocket::build()
        .manage(visitor_counter)
        .mount("/", openapi_get_routes![user, users])
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