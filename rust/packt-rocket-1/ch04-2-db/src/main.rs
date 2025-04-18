#[macro_use]
extern crate rocket;

use rocket::http::{ContentType, Header, Status};
use rocket::response::Responder;
use rocket::serde::json::Json;
use rocket::serde::{Deserialize, Serialize};
use rocket::{request::FromParam, response, Data, Orbit, Request, Response};
use rocket::{Build, Rocket};
use rocket_okapi::gen::OpenApiGenerator;
use rocket_okapi::okapi::openapi3::Responses;
use rocket_okapi::okapi::{schemars, schemars::JsonSchema};
use rocket_okapi::response::OpenApiResponderInner;
use rocket_okapi::swagger_ui::{make_swagger_ui, SwaggerUIConfig};
use rocket_okapi::{openapi, openapi_get_routes};
use std::sync::atomic::{AtomicU64, Ordering};
use std::vec::Vec;
use rocket::fairing::{Fairing, Info, Kind};
use sqlx::{FromRow, SqlitePool};
use sqlx::sqlite::SqlitePoolOptions;
use uuid::Uuid as UUID;

#[derive(Serialize, Deserialize, FromForm, JsonSchema, Debug)]
struct Config {
    database_url: String,
}

#[derive(Serialize, Deserialize, FromForm, JsonSchema, Debug)]
struct Filters {
    age: u8,
    active: bool,
}

#[derive(Serialize, Deserialize, FromForm, JsonSchema, Debug)]
#[schemars(example = "name_grade_example")]
struct NameGrade<'r> {
    name: &'r str,
    grade: u8,
}

impl<'r> FromParam<'r> for NameGrade<'r> {
    type Error = &'static str;

    fn from_param(param: &'r str) -> Result<Self, Self::Error> {
        const ERROR_MESSAGE: Result<NameGrade, &'static str> = Err("Error parsing user parameter");

        let name_grade_vec: Vec<&'r str> = param.split('_').collect();
        match name_grade_vec.len() {
            2 => match name_grade_vec[1].parse::<u8>() {
                Ok(n) => Ok(Self {
                    name: name_grade_vec[0],
                    grade: n,
                }),
                Err(_) => ERROR_MESSAGE,
            },
            _ => ERROR_MESSAGE,
        }
    }
}

fn name_grade_example() -> String {
    String::from("\"John_1\"")
}

#[derive(Serialize, Deserialize, FromForm, JsonSchema, Debug)]
#[schemars(example = "example_uuid")]
struct Uuid<'r>(&'r str);

fn example_uuid() -> String {
    String::from("2a1d0e01-a0cd-4e47-b785-8a5c8ed5bafd")
}

impl<'r> FromParam<'r> for Uuid<'r> {
    type Error = ();

    fn from_param(param: &'r str) -> Result<Self, Self::Error> {
        Ok(Uuid(param))
    }
}

#[derive(Serialize, Deserialize, JsonSchema, Debug, FromRow)]
#[sqlx(rename_all = "camelCase")]
struct User {
    uuid: String,
    name: String,
    age: i64,
    grade: i64,
    active: bool,
    // #[sqlx(rename="active")]
    // present: bool,
    // #[sqlx(default)]
    // not_in_database: String
}

impl<'r> Responder<'r, 'r> for User {
    fn respond_to(self, req: &'r Request<'_>) -> response::Result<'r> {
        Response::
        build_from(Json(&self).respond_to(req)?)
            .raw_header("X-USER-ID", self.uuid)
            .header(ContentType::JSON)
            .ok()
    }
}

impl<'r> OpenApiResponderInner for User {
    fn responses(gen: &mut OpenApiGenerator) -> rocket_okapi::Result<Responses> {
        <Json<&User>>::responses(gen)
    }
}

#[derive(Serialize, JsonSchema, Debug)]
struct Users(Vec<User>);

impl<'r> Responder<'r, 'r> for Users {
    fn respond_to(self, req: &'r Request<'_>) -> response::Result<'r> {
        Response::build_from(Json(self).respond_to(req)?)
            .header(ContentType::JSON)
            .ok()
    }
}

impl<'r> OpenApiResponderInner for Users {
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

#[rocket::async_trait]
impl Fairing for VisitorCounter {
    fn info(&self) -> Info {
        Info {
            name: "Visit Counter",
            kind: Kind::Ignite | Kind::Liftoff | Kind::Request,
        }
    }

    async fn on_ignite(&self, rocket: Rocket<Build>) -> rocket::fairing::Result {
        println!("setting up visitor counter");
        Ok(rocket)
    }

    async fn on_liftoff(&self, _rocket: &Rocket<Orbit>) {
        println!("finish serring up visitor counter");
    }

    async fn on_request(&self, _req: &mut Request<'_>, _data: &mut Data<'_>) {
        self.increment_counter();
    }
}

const X_TRACE_ID: &str = "X-TRACE-ID";
struct XTraceId {}

#[rocket::async_trait]
impl Fairing for XTraceId {
    fn info(&self) -> Info {
        Info {
            name: "X-TRACE-ID Injector",
            kind: Kind::Request | Kind::Response,
        }
    }

    async fn on_request(&self, req: &mut Request<'_>, _data: &mut Data<'_>) {
        let header = Header::new(X_TRACE_ID, UUID::new_v4().to_string());
        req.add_header(header);
    }

    async fn on_response<'r>(&self, req: &'r Request<'_>, res: &mut Response<'r>) {
        let header = req.headers().get_one(X_TRACE_ID).unwrap();
        res.set_header(Header::new(X_TRACE_ID, header));
    }
}


#[openapi(tag = "Users")]
#[get("/user/<uuid>", rank = 1)]
async fn user(
    // counter: &State<VisitorCounter>,
    pool: &rocket::State<SqlitePool>, uuid: Uuid<'_>) -> Result<User, Status> {

    // counter.visitor.fetch_add(1, Ordering::Relaxed);
    // counter.increment_counter();
    // println!("visitors: {}", counter.visitor.load(Ordering::Relaxed));

    let uuid_param = uuid.0.to_string();

    // USERS.get(uuid.0.to_string().as_str())
    let user = sqlx::query_as!(User, r#"SELECT * FROM users WHERE uuid = $1"#, uuid_param).fetch_one(pool.inner()).await;

    user.map_err(|_| Status::NotFound)
}

#[openapi(tag = "Users")]
#[get("/users/<name_grade>?<filters..>", rank = 1)]
async fn users(
    // counter: &State<VisitorCounter>,
    pool: &rocket::State<SqlitePool>, name_grade: NameGrade<'_>, filters: Option<Filters>,) -> Result<Users, Status> {

    // counter.visitor.fetch_add(1, Ordering::Relaxed);
    // counter.increment_counter();
    // println!("visitors: {}", counter.visitor.load(Ordering::Relaxed));

    // Users(USERS.values().collect())

    let mut query_str = String::from("SELECT * FROM users WHERE name LIKE $1 AND grade = $2");

    if filters.is_some() {
        query_str.push_str(" AND age = $3 AND active = $4");
    }

    let mut query = sqlx::query_as::<_, User>(&query_str)
        .bind(format!("%{}%", &name_grade.name))
        .bind(name_grade.grade as i16)
    ;

    if let Some(fts) = &filters {
        query = query.bind(fts.age as i16).bind(fts.active);
    }

    let unwrapped_users = query.fetch_all(pool.inner()).await;
    let users: Vec<User> = unwrapped_users.map_err(|_| Status::InternalServerError)?;

    if users.is_empty() {
        Err(Status::NotFound)
    } else {
        Ok(Users(users))
    }
}

#[launch]
async fn rocket() -> Rocket<Build> {
    let visitor_counter = VisitorCounter {
        visitor: AtomicU64::new(0),
    };

    let x_trace_id = XTraceId {};

    let rocket_frame = rocket::build();

    let config: Config = rocket_frame
        .figment()
        .extract()
        .expect("Incorrect Rocket.toml configuration");

    let pool = SqlitePoolOptions::new()
        .max_connections(5)
        .connect(&config.database_url)
        .await
        .expect("Unable to connect to database");

    rocket_frame
        .manage(pool)
        .attach(visitor_counter)
        .attach(x_trace_id)
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