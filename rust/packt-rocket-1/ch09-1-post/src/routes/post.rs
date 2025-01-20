use super::HtmlResponse;
use crate::models::pagination::Pagination;
use crate::models::post::{Post, ShowPost};
use crate::models::post_type::PostType;
use crate::models::user::User;
// use crate::traits::DisplayPostContent;
use rocket::form::Form;
use rocket::http::Status;
// use rocket::serde::Serialize;
use rocket_dyn_templates::tera::Context;
use rocket_dyn_templates::{context, Template};
use sqlx::SqlitePool;

#[get("/users/<user_uuid>/posts/<uuid>", format = "text/html")]
pub async fn get_post(pool: &rocket::State<SqlitePool>, user_uuid: &str, uuid: &str) -> HtmlResponse {
    let user = User::find(pool, user_uuid).await.map_err(|e| e.status)?;

    let post = Post::find(pool, uuid).await.map_err(|e| e.status)?;

    if post.user_uuid != user.uuid { return Err(Status::InternalServerError); };

    /**
    * way one
    */
    // let mut post_html = String::new();
    // match post.post_type {
    //     PostType::Text => post_html = post.to_text().raw_html(),
    //     PostType::Photo => post_html = post.to_photo().raw_html(),
    //     PostType::Video => post_html = post.to_video().raw_html(),
    // }
    //
    // Ok(Template::render("posts/show", context! {
    //     user,
    //     post: context!{
    //         post_html
    //     }
    // }))

    // #[derive(Serialize)]
    // struct Context {
    //     user: User,
    //     post: ShowPost,
    // }
    //
    // fn create_context<T>(user: User, media: &T) -> Context
    // where
    //     T: crate::traits::DisplayPostContent + ?Sized,
    // {
    //     Context {
    //         user,
    //         post: ShowPost {
    //             post_html: media.raw_html()
    //         },
    //     }
    // }
    //
    // let media = post.to_media();
    // let context = create_context(user, &*media);


    let context = context! { user, post: &(post.to_show())};

    Ok(Template::render("posts/show", context))
}

#[get("/users/<user_uuid>/posts?<pagination>", format = "text/html")]
pub async fn get_posts(pool: &rocket::State<SqlitePool>, user_uuid: &str, pagination: Option<Pagination>) -> HtmlResponse {
    let user = User::find(pool, user_uuid).await.map_err(|e| e.status)?;

    let (posts, new_pagination) = Post::find_all(pool, user_uuid, pagination).await.map_err(|e| e.status)?;

    // #[derive(Serialize)]
    // struct ShowPost {
    //     uuid: String,
    //     post_html: String,
    // }
    //
    // let show_posts: Vec<ShowPost> = posts.into_iter().map(|post| {
    //     ShowPost {
    //         uuid: String::from(&post.uuid),
    //         post_html: post.to_html(),
    //     }
    // }).collect();

    let show_posts: Vec<ShowPost> = posts.into_iter().map(|p| p.to_show()).collect();

    Ok(Template::render("posts/index", context! {
        user,
        posts: &show_posts,
        pagination: new_pagination.map(|pg|pg.to_context())
    }))
}

#[post("/users/<_user_uuid>/posts", format = "text/html", data = "<_upload>")]
pub async fn create_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _upload: Form<Post>) -> HtmlResponse {
    todo!("will implement later");
}

#[delete("/users/<_user_uuid>/posts/<_uuid>", format = "text/html")]
pub async fn delete_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}
