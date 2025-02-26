use std::fmt::format;
use std::fs::File;
use std::io::{BufReader, Read};
use std::ops::Deref;
use std::path::Path;
use image::{DynamicImage, ExtendedColorType, ImageEncoder, ImageReader};
use image::codecs::jpeg::JpegEncoder;
use super::HtmlResponse;
use crate::models::pagination::Pagination;
use crate::models::post::{NewPost, Post, ShowPost};
use crate::models::post_type::PostType;
use crate::models::user::User;
// use rocket::data::ByteUnit;
// use crate::traits::DisplayPostContent;
use rocket::form::Form;
use rocket::http::Status;
use rocket::request::FlashMessage;
use rocket::response::{Flash, Redirect};
use rocket::Data;
// use rocket::serde::Serialize;
use rocket_dyn_templates::{context, Template};
use sqlx::SqlitePool;

// const TEXT_LIMIT: ByteUnit = ByteUnit::Kibibyte(64);

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
// pub async fn create_post(pool: &rocket::State<SqlitePool>, user_uuid: &str, content_type: RawContentType<'_>, upload: Data<'_>) -> Result<Flash<Redirect>, Flash<Redirect>> {
pub async fn create_post<'r>(pool: &rocket::State<SqlitePool>, user_uuid: &str, mut upload: Form<NewPost<'r>>) -> Result<Flash<Redirect>, Flash<Redirect>> {
    let create_err = |e: Option<&str>| {
        Flash::error(
            Redirect::to(format!("/users/{}/posts", user_uuid)),
            format!("Something went wrong creating a post. Please try later: {}", e.unwrap()),
        )
    };

    // let boundary = multer::parse_boundary(content_type.0).map_err(|_| create_err())?;
    //
    // let upload_stream = upload.open(TEXT_LIMIT);
    // let mut multipart = Multipart::new(tokio_util::io::ReaderStream::new(upload_stream), boundary);
    // let mut text_post = String::new();
    //
    // while let Some(mut field) = multipart.next_field().await.map_err(|_| create_err())? {
    //     let field_name = field.name();
    //     let file_name = field.file_name();
    //     let content_type = field.content_type();
    //
    //     println!("Fieldname: {:?}, Filename: {:?}, content-type {:?}", field_name, file_name, content_type);
    //
    //     while let Some(chunk) = field.chunk().await.map_err(|_| create_err())? {
    //         // text_post.push_str(&String::from_utf8_lossy(&chunk));
    //         text_post.push_str(std::str::from_utf8(chunk.as_ref()).unwrap());
    //     }
    // }

    if upload.file.content_type().is_none() {
        return Err(create_err(None));
    }

    let file_uuid = uuid::Uuid::new_v4().to_string();
    let ext = upload.file.content_type().unwrap().extension().unwrap();
    let tmp_filename = format!("/tmp/{}.{}", &file_uuid, &ext);

    upload.file.persist_to(tmp_filename).await.map_err(|e| create_err(Some(e.to_string().as_str())))?;

    let mut content = String::new();
    let mut post_type = PostType::Text;

    let mt = upload.file.content_type().unwrap().deref();
    if mt.is_text() {
        let orig_path = upload.file.path().unwrap().to_string_lossy().to_string();
        let mut text_content = vec![];

        let _ = File::open(orig_path).map_err(|e| create_err(Some(e.to_string().as_str())))?.read(&mut text_content).map_err(|e| create_err(Some(e.to_string().as_str())))?;
        content.push_str(std::str::from_utf8(&text_content).unwrap());
    } else if mt.is_bmp() || mt.is_jpeg() || mt.is_png() || mt.is_gif() {
        post_type = PostType::Photo;

        let orig_path = upload.file.path().unwrap().to_string_lossy().to_string();
        let dest_filename = format!("{}.jpg", file_uuid);
        content.push_str(format!("/assets/{}", &dest_filename).as_str());

        let orig_file = File::open(orig_path).map_err(|e| create_err(Some(e.to_string().as_str())))?;
        let file_reader = BufReader::new(orig_file);
        let mut image: DynamicImage = ImageReader::new(file_reader)
            .with_guessed_format()
            .map_err(|e| create_err(Some(e.to_string().as_str())))?
            .decode()
            .map_err(
                |e| {
                    println!("Failed to decode image: {}", e);

                    create_err(Some(e.to_string().as_str()))
                }
            )?;

        let dest_path = Path::new(rocket::fs::relative!("static")).join(&dest_filename);
        let mut file_writer = File::create(dest_path).map_err(|e| create_err(Some(e.to_string().as_str())))?;
        let encoder = JpegEncoder::new_with_quality(&mut file_writer, 75);

        
        if  mt.is_png() {
            image = DynamicImage::from(image.into_rgb8());
        }

        encoder.write_image(
                image.as_bytes(),
                image.width(),
                image.height(),
                ExtendedColorType::from(image.color()),
            )
            .map_err(
                |e| {
                    println!("Failed to encode image: {}", e);

                    create_err(Some(e.to_string().as_str()))
                }
            )?;
    } else if mt.is_svg() {
        post_type = PostType::Photo;

        let dest_filename = format!("{}.svg", file_uuid);
        content.push_str(format!("/assets/{}", &dest_filename).as_str());
        let dest_path = Path::new(rocket::fs::relative!("static")).join(&dest_filename);

        upload.file.move_copy_to(&dest_path).await.map_err(|e| create_err(Some(e.to_string().as_str())))?;
    } else {
        return Err(create_err(None));
    }

    // Post::create(pool, user_uuid, PostType::Text, &text_post).await.map_err(|_| create_err())?;
    Post::create(pool, user_uuid, post_type, &content).await.map_err(|e| create_err(Some(e.to_string().as_str())))?;

    Ok(Flash::success(
        Redirect::to(format!("/users/{}/posts", user_uuid)),
        "Successfully created a post.",
    ))
}

#[delete("/users/<_user_uuid>/posts/<_uuid>", format = "text/html")]
pub async fn delete_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}
