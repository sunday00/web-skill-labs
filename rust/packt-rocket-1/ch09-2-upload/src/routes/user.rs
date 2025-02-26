use super::HtmlResponse;
use crate::fairings::csrf::Token as CsrfToken;
use crate::models::pagination::Pagination;
use crate::models::user::{EditedUser, GetUser, NewUser, User};
use rocket::form::{Contextual, Form};
use rocket::request::FlashMessage;
use rocket::response::{Flash, Redirect};
use rocket_dyn_templates::{context, Template};
use sqlx::SqlitePool;

#[get("/users/<uuid>", format = "text/html")]
pub async fn get_user(pool: &rocket::State<SqlitePool>, uuid: &str, flash: Option<FlashMessage<'_>>) -> HtmlResponse {
    let user = User::find(pool, uuid).await.map_err(|e| e.status)?;

    Ok(Template::render("users/show", &GetUser {
        user,
        flash: flash.map(|f| String::from(f.message())),
    }))
}

#[get("/users?<pagination>", format = "text/html")]
pub async fn get_users(pool: &rocket::State<SqlitePool>, pagination: Option<Pagination>, flash: Option<FlashMessage<'_>>) -> HtmlResponse {
    let (users, new_pagination) = User::find_all(pool, pagination).await.map_err(|e| e.status)?;

    Ok(Template::render("users/index", context! {
        users, pagination: new_pagination.map(|pg| pg.to_context()), flash: flash.map(|f| String::from(f.message())),
    }))
}

#[get("/users/new", format = "text/html")]
pub async fn new_user(flash: Option<FlashMessage<'_>>, csrf_token: CsrfToken) -> HtmlResponse {
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

#[post("/users/delete/<uuid>", format = "application/x-www-form-urlencoded", rank = 2)]
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