use super::HtmlResponse;
use crate::models::pagination::Pagination;
use crate::models::post::Post;
use rocket::form::Form;
use sqlx::SqlitePool;

#[get("/users/<_user_uuid>/posts/<_uuid>", format = "text/html")]
pub async fn get_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}

#[get("/users/<_user_uuid>/posts?<_pagination>", format = "text/html")]
pub async fn get_posts(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _pagination: Option<Pagination>) -> HtmlResponse {
    todo!("will implement later");
}

#[post("/users/<_user_uuid>/posts", format = "text/html", data = "<_upload>")]
pub async fn create_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _upload: Form<Post>) -> HtmlResponse {
    todo!("will implement later");
}

#[delete("/users/<_user_uuid>/posts/<_uuid>", format = "text/html")]
pub async fn delete_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}