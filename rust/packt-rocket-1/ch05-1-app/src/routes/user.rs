use super::HtmlResponse;
use crate::models::pagination::Pagination;
use crate::models::user::User;
use rocket::form::Form;
use sqlx::SqlitePool;

#[get("/users/<_uuid>", format = "text/html")]
pub async fn get_user(_pool: &rocket::State<SqlitePool>, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}

#[get("/users?<_pagination>", format = "text/html")]
pub async fn get_users(_pool: &rocket::State<SqlitePool>, _pagination: Option<Pagination>) -> HtmlResponse {
    todo!("will implement later");
}

#[get("/users/new", format = "text/html")]
pub async fn new_user(_pool: &rocket::State<SqlitePool>) -> HtmlResponse {
    todo!("will implement later");
}

#[post("/users", format = "text/html", data = "<_user>")]
pub async fn create_user(_pool: &rocket::State<SqlitePool>, _user: Form<User>) -> HtmlResponse {
    todo!("will implement later");
}

#[get("/users/edit/<_uuid>", format = "text/html")]
pub async fn edit_user(_pool: &rocket::State<SqlitePool>, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}

#[put("/users/<_uuid>", format = "text/html", data = "<_user>")]
pub async fn put_user(_pool: &rocket::State<SqlitePool>, _uuid: &str, _user: Form<User>) -> HtmlResponse {
    todo!("will implement later");
}

#[patch("/users/<_uuid>", format = "text/html", data = "<_user>")]
pub async fn patch_user(_pool: &rocket::State<SqlitePool>, _uuid: &str, _user: Form<User>) -> HtmlResponse {
    todo!("will implement later");
}

#[delete("/users/<_uuid>", format = "text/html")]
pub async fn delete_user(_pool: &rocket::State<SqlitePool>, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}