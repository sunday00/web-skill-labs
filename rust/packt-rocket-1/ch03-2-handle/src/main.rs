#[macro_use]
extern crate rocket;

use lazy_static::lazy_static;
use rocket::request::FromParam;
use rocket::{serde::{json::Json, Deserialize, Serialize}, Build, Rocket};
use rocket_okapi::okapi::{schemars, schemars::JsonSchema};
use rocket_okapi::{openapi, openapi_get_routes, swagger_ui::*};
use std::collections::HashMap;

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

#[derive(FromForm, Serialize, Deserialize, JsonSchema, Debug)]
struct Filters {
    age: u8,
    active: bool,
}

#[derive(FromForm, Serialize, Deserialize, JsonSchema, Debug)]
struct User {
    uuid: Uuid,
    name: String,
    age: u8,
    grade: u8,
    active: bool,
}

lazy_static! {
    static ref USERS: HashMap<&'static str, User> = {
        let mut map = HashMap::new();
        map.insert("3e3dd4ae-3c37-40c6-aa64-7061f284ce28", User {
            // uuid: String::from("3e3dd4ae-3c37-40c6-aa64-7061f284ce28"),
            uuid: Uuid(String::from("3e3dd4ae-3c37-40c6-aa64-7061f284ce28")),
            // uuid: Uuid("3e3dd4ae-3c37-40c6-aa64-7061f284ce28"),
            name: String::from("John Doe"),
            age: 18,
            grade: 1,
            active: true,
        });

        map
    };
}

#[openapi(tag = "Users")]
#[get("/user/<uuid>", rank = 1, format = "text/plain")]
fn get_user(uuid: Uuid) -> String {
    let user = USERS.get(uuid.0.as_str());

    match user {
        Some(u) => format!("Found user {:?}", u),
        // None => "No user".to_string(),
        None => String::from("Not found"),
    }
}

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

#[derive(FromForm, Serialize, Deserialize, JsonSchema, Debug)]
#[schemars(example = "name_grade_example")]
struct NameGrade<'r> {
    name: &'r str,
    grade: u8,
}

fn name_grade_example() -> String {
    String::from("\"John_1\"")
}

impl<'r> FromParam<'r> for NameGrade<'r> {
    type Error = &'static str;

    fn from_param(param: &'r str) -> Result<Self, Self::Error> {
        const ERROR_MESSAGE: Result<NameGrade, &'static str> = Err("Error parsing user parameter");

        let name_grade_vec: Vec<&'r str> = param.split('_').collect();

        match name_grade_vec.len() {
            2 => match name_grade_vec[1].parse::<u8>() {
                Ok(n) => Ok(Self { name: name_grade_vec[0], grade: n }),
                Err(_) => ERROR_MESSAGE
            },
            _ => ERROR_MESSAGE
        }
    }
}

#[openapi(tag = "Users")]
#[get("/users/complicated/<name_grade>?<filters..>")]
fn users_by_name_grade(name_grade: NameGrade, filters: Option<Filters>) -> String {
    let users: Vec<&User> = USERS.values()
        .filter(|u| u.name.contains(&name_grade.name) && u.grade == name_grade.grade)
        // .filter(|u| u.age == filters.age && u.active == filters.active)
        .filter(|u| {
            if let Some(fts) = &filters {
                u.age == fts.age && u.active == fts.active
            } else {
                true
            }
        })
        .collect();

    if users.len() > 0 {
        users.iter().map(|u| u.name.to_string()).collect::<Vec<String>>().join("")
    } else {
        String::from("Not found")
    }
}


// ======
#[launch]
fn rocket() -> Rocket<Build> {
    // rocket::build().mount("/", routes![user, users])

    rocket::build().mount("/", openapi_get_routes![get_user, users, create_users, first, second, users_by_name_grade])
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