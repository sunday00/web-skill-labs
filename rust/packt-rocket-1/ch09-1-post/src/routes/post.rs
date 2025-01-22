use super::HtmlResponse;
use crate::guards::RawContentType;
use crate::models::pagination::Pagination;
use crate::models::post::{Post, ShowPost};
use crate::models::post_type::PostType;
use crate::models::user::User;
use multer::Multipart;
use rocket::data::ByteUnit;
// use crate::traits::DisplayPostContent;
use rocket::form::Form;
use rocket::http::Status;
use rocket::request::FlashMessage;
use rocket::response::{Flash, Redirect};
use rocket::Data;
// use rocket::serde::Serialize;
use rocket_dyn_templates::{context, Template};
use sqlx::SqlitePool;

const TEXT_LIMIT: ByteUnit = ByteUnit::Kibibyte(64);

#[get("/users/<user_uuid>/posts/<uuid>", format = "text/html")]
pub async fn get_post(pool: &rocket::State<SqlitePool>, user_uuid: &str, uuid: &str) -> HtmlResponse {
    let user = User::find(pool, user_uuid).await.map_err(|e| e.status)?;

    let post = Post::find(pool, uuid).await.map_err(|e| e.status)?;

    if post.user_uuid != user.uuid { return Err(Status::InternalServerError); };


    let context = context! { user, post: &(post.to_show())};

    Ok(Template::render("posts/show", context))
}

#[get("/users/<user_uuid>/posts?<pagination>", format = "text/html")]
pub async fn get_posts(pool: &rocket::State<SqlitePool>, user_uuid: &str, pagination: Option<Pagination>, flash: Option<FlashMessage<'_>>) -> HtmlResponse {
    let flash_message = flash.map(|f| String::from(f.message()));

    let user = User::find(pool, user_uuid).await.map_err(|e| e.status)?;

    let (posts, new_pagination) = Post::find_all(pool, user_uuid, pagination).await.map_err(|e| e.status)?;

    let show_posts: Vec<ShowPost> = posts.into_iter().map(|p| p.to_show()).collect();

    Ok(Template::render("posts/index", context! {
        flash: flash_message,
        user,
        posts: &show_posts,
        pagination: new_pagination.map(|pg|pg.to_context())
    }))
}

#[post("/users/<user_uuid>/posts", format = "multipart/form-data", data = "<upload>", rank = 1)]
pub async fn create_post(pool: &rocket::State<SqlitePool>, user_uuid: &str, content_type: RawContentType<'_>, upload: Data<'_>) -> Result<Flash<Redirect>, Flash<Redirect>> {
    let create_err = || {
        Flash::error(
            Redirect::to(format!("/users/{}/posts", user_uuid)),
            "Something went wrong creating a post. Please try later",
        )
    };

    let boundary = multer::parse_boundary(content_type.0).map_err(|_| create_err())?;

    let upload_stream = upload.open(TEXT_LIMIT);
    let mut multipart = Multipart::new(tokio_util::io::ReaderStream::new(upload_stream), boundary);
    let mut text_post = String::new();

    while let Some(mut field) = multipart.next_field().await.map_err(|_| create_err())? {
        let field_name = field.name();
        let file_name = field.file_name();
        let content_type = field.content_type();

        println!("Fieldname: {:?}, Filename: {:?}, content-type {:?}", field_name, file_name, content_type);

        while let Some(chunk) = field.chunk().await.map_err(|_| create_err())? {
            // text_post.push_str(&String::from_utf8_lossy(&chunk));
            text_post.push_str(std::str::from_utf8(chunk.as_ref()).unwrap());
        }
    }

    Post::create(pool, user_uuid, PostType::Text, &text_post).await.map_err(|_| create_err())?;

    Ok(Flash::success(
        Redirect::to(format!("/users/{}/posts", user_uuid)),
        "Successfully created a post.",
    ))
}

#[delete("/users/<_user_uuid>/posts/<_uuid>", format = "text/html")]
pub async fn delete_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}
