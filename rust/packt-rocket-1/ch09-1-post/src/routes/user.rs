use super::HtmlResponse;
use crate::fairings::csrf::Token as CsrfToken;
use crate::models::pagination::Pagination;
use crate::models::user::{EditedUser, GetUser, NewUser, User};
use rocket::form::{Contextual, Form};
use rocket::http::Status;
use rocket::request::FlashMessage;
use rocket::response::content::RawHtml;
use rocket::response::{Flash, Redirect};
use rocket_dyn_templates::{context, Template};
use sqlx::SqlitePool;
use std::fmt::format;

// const USER_HTML_PREFIX: &str = r#"<!DOCTYPE html>
// <html lang="en">
// <head>
// <meta charset="utf-8" />
// <title>Our Application User</title>
// <link rel="icon" type="image/x-icon" href="/assets/favicon.png">
// </head>
// <body>"#;
//
// const USER_HTML_SUFFIX: &str = r#"</body>
// </html>"#;

#[get("/users/<uuid>", format = "text/html")]
pub async fn get_user(pool: &rocket::State<SqlitePool>, uuid: &str, flash: Option<FlashMessage<'_>>) -> HtmlResponse {
    let user = User::find(pool, uuid).await.map_err(|e| e.status)?;

    // let mut html_string = String::from(USER_HTML_PREFIX);
    //
    // if flash.is_some() {
    //     html_string.push_str(flash.unwrap().message());
    // }
    //
    // html_string.push_str(&user.to_html_string());
    // html_string.push_str(r#"<div style="display: flex; gap: 0.5em;">"#);
    // html_string.push_str(format!(r#"<a href="/users/edit/{}">Edit User</a>"#, user.uuid).as_ref());
    // html_string.push_str(r#"<a href="/users">User List</a>"#);
    // html_string.push_str(r#"</div>"#);
    // html_string.push_str(USER_HTML_SUFFIX);
    // Ok(RawHtml(html_string))

    Ok(Template::render("users/show", &GetUser {
        user,
        flash: flash.map(|f| String::from(f.message())),
    }))
}

#[get("/users?<pagination>", format = "text/html")]
pub async fn get_users(pool: &rocket::State<SqlitePool>, pagination: Option<Pagination>, flash: Option<FlashMessage<'_>>) -> HtmlResponse {
    let (users, new_pagination) = User::find_all(pool, pagination).await.map_err(|e| e.status)?;

    // let mut html_string = String::from(USER_HTML_PREFIX);

    // if flash.is_some() {
    //     html_string.push_str(format!(r#"<div style="color: red;">{}</div>"#, flash.unwrap().message()).as_ref());
    // }
    //
    // for user in users {
    //     html_string.push_str(r#"<div style="display: flex; gap: 0.5em;">"#);
    //     html_string.push_str(&user.to_mini_string());
    //     html_string.push_str(format!(r#"<a href="/users/{}">show</a>"#, user.uuid).as_ref());
    //     html_string.push_str(format!(r#"<a href="/users/edit/{}">edit</a>"#, user.uuid).as_ref());
    //     html_string.push_str(format!(r#"<form accept-charset="UTF-8" action="/users/delete/{}" autocomplete="off" method="POST">
    //         <button type="submit" value="Submit">Delete</button>
    //     </form>"#, user.uuid).as_ref());
    //     html_string.push_str(r#"</div>"#);
    // }
    //
    // if let Some(pg) = new_pagination {
    //     html_string.push_str(
    //         format!(
    //             r#"<a href="/users?pagination.next={}&pagination.limit={}">Next</a><br/>"#,
    //             &(pg.next.0).timestamp_nanos(),
    //             &pg.limit
    //         ).as_ref(),
    //     )
    // }
    //
    // html_string.push_str(r#"<a href="/users/new">new</a>"#);
    // html_string.push_str(USER_HTML_SUFFIX);
    //
    // Ok(RawHtml(html_string))

    Ok(Template::render("users/index", context! {
        users, pagination: new_pagination.map(|pg| pg.to_context()), flash: flash.map(|f| String::from(f.message())),
    }))
}

#[get("/users/new", format = "text/html")]
pub async fn new_user(flash: Option<FlashMessage<'_>>, csrf_token: CsrfToken) -> HtmlResponse {
    //     let mut html_string = String::from(USER_HTML_PREFIX);
    //
    //     if flash.is_some() {
    //         html_string.push_str(format!(r#"<div style="color: red;">{}</div>"#, flash.unwrap().message()).as_ref());
    //     }
    //
    //     html_string.push_str(
    //         r#"<form accept-charset="UTF-8" action="/users" autocomplete="off" method="POST">
    //     <div>
    //         <label for="username">Username:</label>
    //         <input name="username" type="text"/>
    //     </div>
    //     <div>
    //         <label for="email">Email:</label>
    //         <input name="email" type="email"/>
    //     </div>
    //     <div>
    //         <label for="password">Password:</label>
    //         <input name="password" type="password"/>
    //     </div>
    //     <div>
    //         <label for="password_confirmation">Password Confirmation:</label>
    //         <input name="password_confirmation" type="password"/>
    //     </div>
    //     <div>
    //         <label for="description">Tell us a little bit more about yourself:</label>
    //         <textarea name="description"></textarea>
    //     </div>
    //     <button type="submit" value="Submit">Submit</button>
    // </form>"#
    //     );
    //
    //     html_string.push_str(USER_HTML_SUFFIX);
    // Ok(RawHtml(html_string))

    Ok(Template::render("users/form", context!(
        edit: false,
        form_url: "/users",
        legend: "New User",
        flash: flash.map(|f| String::from(f.message())),
        csrf_token: csrf_token,
    )))
}

#[post("/users", format = "application/x-www-form-urlencoded", data = "<user_context>")]
pub async fn create_user<'r>(pool: &rocket::State<SqlitePool>, user_context: Form<Contextual<'r, NewUser<'r>>>, csrf_token: CsrfToken) -> Result<Flash<Redirect>, Flash<Redirect>> {
    if user_context.value.is_none() {
        let error_message = format!(
            r#"<span style="color: red;">{}</span>"#,
            user_context.context.errors().map(|e| e.to_string()).collect::<Vec<_>>().join("<br />")
        );

        return Err(Flash::error(Redirect::to("/users/new"), error_message));
    }

    let new_user = user_context.value.as_ref().unwrap();

    csrf_token.verify(&new_user.authenticity_token).map_err(|_| {
        Flash::error(
            Redirect::to("/users/new"),
            r#"<span style="color: red;">Something went wrong when creating user</span>"#,
        )
    })?;

    let user = User::create(pool, new_user).await.map_err(|e| {
        println!("{}", e);
        Flash::error(Redirect::to("/users/new"), format!(r#"<span style="color: red;">{}</span>"#, e.to_string()))
    })?;

    Ok(Flash::success(
        Redirect::to(format!("/users/{}", user.uuid)), r#"<span style="color: green;">Successfully created user</span>"#,
    ))
}

#[get("/users/edit/<uuid>", format = "text/html")]
pub async fn edit_user(pool: &rocket::State<SqlitePool>, uuid: &str, flash: Option<FlashMessage<'_>>, csrf_token: CsrfToken) -> HtmlResponse {
    let user = User::find(pool, uuid).await.map_err(|e| e.status)?;

    //     let mut html_string = String::from(USER_HTML_PREFIX);
    //     if flash.is_some() {
    //         html_string.push_str(format!(r#"<div style="color: red;">{}</div>"#, flash.unwrap().message()).as_ref());
    //     }
    //
    //     html_string.push_str(
    //         format!(
    //             r#"<form accept-charset="UTF-8" action="/users/{}" autocomplete="off" method="POST">
    //     <input type="hidden" name="_METHOD" value="PUT"/>
    //     <div>
    //         <label for="username">Username:</label>
    //         <input name="username" type="text" value="{}"/>
    //     </div>
    //     <div>
    //         <label for="email">Email:</label>
    //         <input name="email" type="email" value="{}"/>
    //     </div>
    //     <div>
    //         <label for="old_password">Old password:</label>
    //         <input name="old_password" type="password"/>
    //     </div>
    //     <div>
    //         <label for="password">New password:</label>
    //         <input name="password" type="password"/>
    //     </div>
    //     <div>
    //         <label for="password_confirmation">Password Confirmation:</label>
    //         <input name="password_confirmation" type="password"/>
    //     </div>
    //     <div>
    //         <label for="description">Tell us a little bit more about yourself:</label>
    //         <textarea name="description">{}</textarea>
    //     </div>
    //     <button type="submit" value="Submit">Submit</button>
    // </form>"#,
    //             &user.uuid,
    //             &user.username,
    //             &user.email,
    //             &user.description.unwrap_or_else(|| "".to_string()),
    //         )
    //             .as_ref(),
    //     );
    //     html_string.push_str(USER_HTML_SUFFIX);
    // Ok(RawHtml(html_string))

    Ok(Template::render("users/form", context!(
        edit: true,
        form_url: format!("/users/{}", &user.uuid ),
        user,
        legend: "Edit User",
        flash: flash.map(|f| String::from(f.message())).unwrap_or_else(|| "".to_string()),
        csrf_token: csrf_token,
    )))
}

#[post("/users/<uuid>", format = "application/x-www-form-urlencoded", data = "<user_context>")]
pub async fn update_user<'r>(pool: &rocket::State<SqlitePool>, uuid: &str, user_context: Form<Contextual<'r, EditedUser<'r>>>, csrf_token: CsrfToken) -> Result<Flash<Redirect>, Flash<Redirect>> {
    if user_context.value.is_none() {
        let error_message = format!(
            r#"<span style="color: red;">{}</span>"#,
            user_context.context.errors()
                .map(|e| e.to_string()).collect::<Vec<_>>().join("<br />")
        );
        return Err(Flash::error(
            Redirect::to(format!("/users/edit/{}", uuid)), error_message,
        ));
    }
    let user_value = user_context.value.as_ref().unwrap();
    match user_value.method {
        "PUT" => put_user(pool, uuid, user_context, csrf_token).await,
        "PATCH" => patch_user(pool, uuid, user_context, csrf_token).await,
        _ => Err(Flash::error(
            Redirect::to(format!("/users/edit/{}", uuid)),
            r#"<span style="color: red;">Something went wrong when updating user. Not supported FORM method.</span>"#,
        ))
    }
}

#[put("/users/<uuid>", format = "application/x-www-form-urlencoded", data = "<user_context>")]
pub async fn put_user<'r>(pool: &rocket::State<SqlitePool>, uuid: &str, user_context: Form<Contextual<'r, EditedUser<'r>>>, csrf_token: CsrfToken) -> Result<Flash<Redirect>, Flash<Redirect>> {
    let user_value = user_context.value.as_ref().unwrap();

    csrf_token
        .verify(&user_value.authenticity_token)
        .map_err(|_| {
            Flash::error(
                Redirect::to(format!("/users/edit/{}", uuid)),
                r#"<span style="color: red;">Something went wrong when updating user</span>"#,
            )
        })?;

    let user = User::update(pool, uuid, user_value).await.map_err(|e| {
        println!("{}", e);
        Flash::error(
            Redirect::to(format!("/users/edit/{}", uuid)),
            format!(r#"<span style="color: red;">{}</span>"#, e),
        )
    })?;
    Ok(Flash::success(
        Redirect::to(format!("/users/{}", user.uuid)),
        r#"<span style="color: green;">Successfully updated user</span>"#,
    ))
}

#[patch("/users/<uuid>", format = "application/x-www-form-urlencoded", data = "<user_context>")]
pub async fn patch_user<'r>(pool: &rocket::State<SqlitePool>, uuid: &str, user_context: Form<Contextual<'r, EditedUser<'r>>>, csrf_token: CsrfToken) -> Result<Flash<Redirect>, Flash<Redirect>> {
    put_user(pool, uuid, user_context, csrf_token).await
}

#[post("/users/delete/<uuid>", format = "application/x-www-form-urlencoded")]
pub async fn delete_user_entry_point(pool: &rocket::State<SqlitePool>, uuid: &str) -> Result<Flash<Redirect>, Flash<Redirect>> {
    delete_user(pool, uuid).await
}

#[delete("/users/<uuid>", format = "application/x-www-form-urlencoded")]
pub async fn delete_user(pool: &rocket::State<SqlitePool>, uuid: &str) -> Result<Flash<Redirect>, Flash<Redirect>> {
    User::destroy(pool, uuid).await.map_err(|e| {
        println!("{}", e);

        Flash::error(
            Redirect::to("/users"),
            format!(r#"<span style="color: red;">{}</span>"#, e),
        )
    })?;

    Ok(Flash::success(
        Redirect::to("/users"),
        r#"<span style="color: green;">Successfully updated user</span>"#,
    ))
}