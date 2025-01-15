use super::HtmlResponse;
use crate::models::pagination::Pagination;
use crate::models::user::{NewUser, User};
use rocket::form::{Contextual, Form};
use rocket::http::Status;
use rocket::request::FlashMessage;
use rocket::response::content::RawHtml;
use rocket::response::{Flash, Redirect};
use sqlx::SqlitePool;

const USER_HTML_PREFIX: &str = r#"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8" />
<title>Our Application User</title>
<link rel="icon" type="image/x-icon" href="/favicon.png">
</head>
<body>"#;

const USER_HTML_SUFFIX: &str = r#"</body>
</html>"#;

#[get("/users/<uuid>", format = "text/html")]
pub async fn get_user(pool: &rocket::State<SqlitePool>, uuid: &str, flash: Option<FlashMessage<'_>>) -> HtmlResponse {
    let user = User::find(pool, uuid).await.map_err(|_| Status::NotFound)?;

    let mut html_string = String::from(USER_HTML_PREFIX);

    if flash.is_some() {
        html_string.push_str(flash.unwrap().message());
    }

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
pub async fn new_user(flash: Option<FlashMessage<'_>>) -> HtmlResponse {
    let mut html_string = String::from(USER_HTML_PREFIX);

    if flash.is_some() {
        html_string.push_str(format!(r#"<div style="color: red;">{}</div>"#, flash.unwrap().message()).as_ref());
    }

    html_string.push_str(
        r#"<form accept-charset="UTF-8" action="/users" autocomplete="off" method="POST">
    <div>
        <label for="username">Username:</label>
        <input name="username" type="text"/>
    </div>
    <div>
        <label for="email">Email:</label>
        <input name="email" type="email"/>
    </div>
    <div>
        <label for="password">Password:</label>
        <input name="password" type="password"/>
    </div>
    <div>
        <label for="password_confirmation">Password Confirmation:</label>
        <input name="password_confirmation" type="password"/>
    </div>
    <div>
        <label for="description">Tell us a little bit more about yourself:</label>
        <textarea name="description"></textarea>
    </div>
    <button type="submit" value="Submit">Submit</button>
</form>"#
    );

    html_string.push_str(USER_HTML_SUFFIX);
    Ok(RawHtml(html_string))
}

#[post("/users", format = "application/x-www-form-urlencoded", data = "<user_context>")]
pub async fn create_user<'r>(pool: &rocket::State<SqlitePool>, user_context: Form<Contextual<'r, NewUser<'r>>>) -> Result<Flash<Redirect>, Flash<Redirect>> {
    if user_context.value.is_none() {
        let error_message = format!(
            "<div>{}</div>",
            user_context.context.errors().map(|e| e.to_string()).collect::<Vec<_>>().join("<br />")
        );

        return Err(Flash::error(Redirect::to("/users/new"), error_message));
    }

    let new_user = user_context.value.as_ref().unwrap();
    let user = User::create(pool, new_user).await.map_err(|e| {
        println!("{}", e);
        Flash::error(Redirect::to("/users/new"), String::from("<h2>Failed to create user</h2>"))
    })?;

    Ok(Flash::success(
        Redirect::to(format!("/users/{}", user.uuid)), "<h2>Successfully created user</h2>",
    ))
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