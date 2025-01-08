#[macro_use]
extern crate rocket;

use rocket::{Build, Rocket, serde::{Serialize, Deserialize, json::Json}};
use rocket_okapi::okapi::{schemars, schemars::JsonSchema};
use rocket_okapi::{openapi, openapi_get_routes, swagger_ui::*};

#[derive(FromForm, Serialize, Deserialize, JsonSchema)]
struct Filters {
    age: u8,
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

#[launch]
fn rocket() -> Rocket<Build> {
    // rocket::build().mount("/", routes![user, users])

    rocket::build().mount("/", openapi_get_routes![users, create_users])
        .mount("/docs/", make_swagger_ui(&SwaggerUIConfig {
            url: "../openapi.json".to_owned(),
            ..Default::default()
        }),)
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