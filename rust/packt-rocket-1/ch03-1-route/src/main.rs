#[macro_use]
extern crate rocket;

use rocket::{serde::{json::Json, Deserialize, Serialize}, Build, Rocket};
use rocket_okapi::okapi::{schemars, schemars::JsonSchema};
use rocket_okapi::{openapi, openapi_get_routes, swagger_ui::*};

#[derive(FromForm, Serialize, Deserialize, JsonSchema, Debug)]
struct Filters {
    age: u8,
    active: bool,
}

#[derive(Debug)]
struct User {
    uuid: String,
    name: String,
    age: u8,
    grade: u8,
    active: bool,
}

#[route(GET, uri = "/user/<uuid>", rank = 1, format = "text/plain")]
fn get_user(uuid: &str) { /* ... */ }

#[openapi(tag = "Users")]
#[get("/users/<grade>?<filters..>")]
fn users(grade: u8, filters: Filters) { /* ... */ }

#[openapi(tag = "Users")]
#[post("/users/<grade>", data = "<filters>")]
fn create_users(grade: u8, filters: Json<Filters>) { /* ... */ }

// ======
#[openapi(tag = "Rank")]
#[get("/<rank>", rank = 1)]
fn first(rank: u8) -> String {
    let result = rank + 10;
    format!("Your rank is, {}!", result)
}

#[openapi(tag = "Rank")]
#[get("/<name>", rank = 2)]
fn second(name: &str) -> String {
    format!("Hello, {}!", name)
}

// ======

#[get("/get", format = "text/plain")]
fn get() -> &'static str {
    "GET Request"
}

#[post("/post", format = "form", data = "<data>")]
fn post(data: Json<Filters>) -> &'static str {
    println!("{:?}", data);

    "POST Request"
}

// ======


// ======
#[launch]
fn rocket() -> Rocket<Build> {
    // rocket::build().mount("/", routes![user, users])

    rocket::build().mount("/", openapi_get_routes![users, create_users, first, second])
        .mount("/docs/", make_swagger_ui(&SwaggerUIConfig {
            url: "../openapi.json".to_owned(),
            ..Default::default()
        }), )
        .mount("/no-s/", routes![get, post])
}


// #[rocket::main]
// async fn main() {
//     let launch_result = rocket::build()
//         .mount(
//             "/",
//             openapi_get_routes![
//                 get_all_users,
//             ],
//         )
//         .mount(
//             "/docs/",
//             make_swagger_ui(&SwaggerUIConfig {
//                 url: "../openapi.json".to_owned(),
//                 ..Default::default()
//             }),
//         )
//         .launch()
//         .await;
//
//     match launch_result {
//         Ok(_) => println!("Rocket shut down gracefully."),
//         Err(err) => println!("Rocket had an error: {}", err),
//     };
// }