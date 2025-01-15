use super::HtmlResponse;
use crate::models::pagination::Pagination;
use crate::models::user::User;
use rocket::form::Form;
use rocket::http::Status;
use rocket::response::content::RawHtml;
use sqlx::SqlitePool;

const USER_HTML_PREFIX: &str = r#"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8" />
<title>Our Application User</title>
</head>
<body>"#;

const USER_HTML_SUFFIX: &str = r#"</body>
</html>"#;

#[get("/users/<uuid>", format = "text/html")]
pub async fn get_user(pool: &rocket::State<SqlitePool>, uuid: &str) -> HtmlResponse {
    let user = User::find(pool, uuid).await.map_err(|_| Status::NotFound)?;

    let mut html_string = String::from(USER_HTML_PREFIX);
    html_string.push_str(&user.to_html_string());
    html_string.push_str(r#"<div style="display: flex; gap: 0.5em;">"#);
    html_string.push_str(format!(r#"<a href="/users/edit/{}">Edit User</a>"#, user.uuid).as_ref());
    html_string.push_str(r#"<a href="/users">User List</a>"#);
    html_string.push_str(r#"</div>"#);
    html_string.push_str(USER_HTML_SUFFIX);
    Ok(RawHtml(html_string))
}

#[get("/users?<pagination>", format = "text/html")]
pub async fn get_users(pool: &rocket::State<SqlitePool>, pagination: Option<Pagination>) -> HtmlResponse {
    let (users, new_pagination) = User::find_all(pool, pagination).await.map_err(|_| Status::NotFound)?;

    let mut html_string = String::from(USER_HTML_PREFIX);

    for user in users {
        html_string.push_str(r#"<div style="display: flex; gap: 0.5em;">"#);
        html_string.push_str(&user.to_mini_string());
        html_string.push_str(format!(r#"<a href="/users/{}">show</a>"#, user.uuid).as_ref());
        html_string.push_str(format!(r#"<a href="/users/edit/{}">edit</a>"#, user.uuid).as_ref());
        html_string.push_str(r#"</div>"#);
    }

    if let Some(pg) = new_pagination {
        html_string.push_str(
            format!(
                r#"<a href="/users?pagination.next={}&pagination.limit={}">Next</a><br/>"#,
                &(pg.next.0).timestamp_nanos(),
                &pg.limit
            ).as_ref(),
        )
    }

    html_string.push_str(r#"<a href="/users/new">new</a>"#);
    html_string.push_str(USER_HTML_SUFFIX);

    Ok(RawHtml(html_string))
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