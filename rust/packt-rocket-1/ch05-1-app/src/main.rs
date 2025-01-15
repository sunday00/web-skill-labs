#[macro_use]
extern crate rocket;

use chrono::{offset::Utc, DateTime};
use rocket::form::{DataField, Form, FromFormField, ValueField};
use rocket::fs::NamedFile;
use rocket::http::Status;
use rocket::response::content::RawHtml;
use rocket::serde::{Deserialize, Serialize};
use rocket::{form, Build, Request, Rocket};
use rocket_okapi::okapi::{schemars, schemars::JsonSchema};
use rocket_okapi::openapi_get_routes;
use rocket_okapi::swagger_ui::{make_swagger_ui, SwaggerUIConfig};
use sqlx::sqlite::SqlitePoolOptions;
use sqlx::{Connection, FromRow, SqlitePool};

// ==== [[[[structures]]]] ===========
// |
// |
// ====datetime====
#[derive(Debug, FromRow)]
struct OurDateTime(DateTime<Utc>);

#[rocket::async_trait]
impl<'r> FromFormField<'r> for OurDateTime {
    fn from_value(_: ValueField<'r>) -> form::Result<'r, Self> {
        todo!("will implement later");
    }

    async fn from_data(_: DataField<'r, '_>) -> form::Result<'r, Self> {
        todo!("will implement later");
    }
}

// ====paginate obj====
#[derive(FromForm)]
struct Pagination {
    cursor: OurDateTime,
    limit: usize,
}

// ====user schema====
#[derive(sqlx::Type, Debug, FromFormField)]
#[repr(i32)]
enum UserStatus {
    Inactive = 0,
    Active = 1,
}

#[derive(Debug, FromRow, FromForm)]
struct User {
    uuid: String,
    username: String,
    email: String,
    password_hash: String,
    description: String,
    status: UserStatus,
    created_at: OurDateTime,
    updated_at: OurDateTime,
}

// ====post schema====
#[derive(sqlx::Type, Debug, FromFormField)]
#[repr(i32)]
enum PostType {
    Text = 0,
    Photo = 1,
    Video = 2,
}
#[derive(FromForm)]
struct Post {
    uuid: String,
    user_uuid: String,
    post_type: PostType,
    content: String,
    created_at: OurDateTime,
}

// ====res dto====
type HtmlResponse = Result<RawHtml<String>, Status>;

trait DisplayPostContent {
    fn raw_html() -> String;
}

struct TextPost(Post);
impl DisplayPostContent for TextPost {
    fn raw_html() -> String {
        todo!("will implement later")
    }
}

struct PhotoPost(Post);
impl DisplayPostContent for PhotoPost {
    fn raw_html() -> String {
        todo!("will implement later")
    }
}

struct VideoPost(Post);
impl DisplayPostContent for VideoPost {
    fn raw_html() -> String {
        todo!("will implement later")
    }
}
// |
// |
// ==== [[[[structures]]]] ===========

// ==== [[[[routes]]]] ===========
// |
// |
// ==== user routes ====
#[get("/users/<_uuid>", format = "text/html")]
async fn get_user(_pool: &rocket::State<SqlitePool>, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}

#[get("/users?<_pagination>", format = "text/html")]
async fn get_users(_pool: &rocket::State<SqlitePool>, _pagination: Option<Pagination>) -> HtmlResponse {
    todo!("will implement later");
}

#[get("/users/new", format = "text/html")]
async fn new_user(_pool: &rocket::State<SqlitePool>) -> HtmlResponse {
    todo!("will implement later");
}

#[post("/users", format = "text/html", data = "<_user>")]
async fn create_user(_pool: &rocket::State<SqlitePool>, _user: Form<User>) -> HtmlResponse {
    todo!("will implement later");
}

#[get("/users/edit/<_uuid>", format = "text/html")]
async fn edit_user(_pool: &rocket::State<SqlitePool>, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}

#[put("/users/<_uuid>", format = "text/html", data = "<_user>")]
async fn put_user(_pool: &rocket::State<SqlitePool>, _uuid: &str, _user: Form<User>) -> HtmlResponse {
    todo!("will implement later");
}

#[patch("/users/<_uuid>", format = "text/html", data = "<_user>")]
async fn patch_user(_pool: &rocket::State<SqlitePool>, _uuid: &str, _user: Form<User>) -> HtmlResponse {
    todo!("will implement later");
}

#[delete("/users/<_uuid>", format = "text/html")]
async fn delete_user(_pool: &rocket::State<SqlitePool>, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}

// ==== post routes ====
#[get("/users/<_user_uuid>/posts/<_uuid>", format = "text/html")]
async fn get_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}

#[get("/users/<_user_uuid>/posts?<_pagination>", format = "text/html")]
async fn get_posts(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _pagination: Option<Pagination>) -> HtmlResponse {
    todo!("will implement later");
}

#[post("/users/<_user_uuid>/posts", format = "text/html", data = "<_upload>")]
async fn create_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _upload: Form<Post>) -> HtmlResponse {
    todo!("will implement later");
}

#[delete("/users/<_user_uuid>/posts/<_uuid>", format = "text/html")]
async fn delete_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}

// ==== file upload ====
#[get("/<_filename>")]
async fn assets(_filename: &str) -> NamedFile {
    todo!("will implement later");
}
// |
// |
// ==== [[[[routes]]]] ===========

// ==== [[[[define errors]]]] ===========
// |
// |
#[catch(404)]
fn not_found(_: &Request) -> RawHtml<String> {
    todo!("will implement later");
}

#[catch(422)]
fn unprocessable_entity(_: &Request) -> RawHtml<String> {
    todo!("will implement later");
}

#[catch(500)]
fn internal_server_error(_: &Request) -> RawHtml<String> {
    todo!("will implement later");
}
// |
// |
// ==== [[[[define errors]]]] ===========

// ==== [[[[config and launch]]]] ===========
// |
// |
#[derive(Serialize, Deserialize, FromForm, JsonSchema, Debug)]
struct Config {
    database_url: String,
}

#[launch]
async fn rocket() -> Rocket<Build> {
    let server = rocket::build();

    let config: Config = server
        .figment()
        .extract()
        .expect("Incorrect Rocket.toml configuration");

    let pool = SqlitePoolOptions::new()
        .max_connections(5)
        .connect(&config.database_url)
        .await
        .expect("Unable to connect to database");

    server.manage(pool)
        // .mount("/", openapi_get_routes![])
        .mount("/", routes![
            get_user, get_users, new_user, create_user, edit_user, put_user, patch_user, delete_user,
            get_post, get_posts, create_post, delete_post,
        ])
        .mount("/assets", routes![assets])
        .mount("/docs/", make_swagger_ui(&SwaggerUIConfig {
            url: "../openapi.json".to_owned(),
            ..Default::default()
        }), )
        .register("/", catchers![not_found, unprocessable_entity, internal_server_error])
}
// |
// |
// ==== [[[[config and launch]]]] ===========