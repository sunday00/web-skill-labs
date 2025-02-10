use super::HtmlResponse;
use crate::errors::our_error::OurError;
use crate::models::pagination::Pagination;
use crate::models::post::{NewPost, Post, ShowPost};
use crate::models::post_type::PostType;
use crate::models::user::User;
use crate::models::worker::Message;
use flume::Sender;
use image::codecs::jpeg::JpegEncoder;
use image::{DynamicImage, ExtendedColorType, ImageEncoder, ImageError, ImageReader};
// use rocket::data::ByteUnit;
// use crate::traits::DisplayPostContent;
use rocket::form::Form;
use rocket::http::Status;
use rocket::request::FlashMessage;
use rocket::response::{Flash, Redirect};
use rocket::{Data, State};
// use rocket::serde::Serialize;
use rocket_dyn_templates::{context, Template};
use sqlx::SqlitePool;
use std::fmt::format;
// use std::fs::File;
use std::io::{BufReader, Cursor, Read};
use std::ops::Deref;
use std::path::Path;
use tokio::fs::File;
use tokio::io::AsyncReadExt;

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
pub async fn create_post<'r>(
    pool: &rocket::State<SqlitePool>, user_uuid: &str, mut upload: Form<NewPost<'r>>, tx: &State<Sender<Message>>,
) -> Result<Flash<Redirect>, Flash<Redirect>> {
    let create_err = |e: Option<&str>| {
        Flash::error(
            Redirect::to(format!("/users/{}/posts", user_uuid)),
            format!("Something went wrong creating a post. Please try later: {}", e.unwrap()),
        )
    };

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
    let mut wm = Message::new();
    let mut is_video = false;

    if mt.is_text() {
        let orig_path = upload.file.path().unwrap().to_string_lossy().to_string();
        let mut text_content = vec![];

        let _ = File::open(orig_path)
            .await
            .map_err(|e| create_err(Some(e.to_string().as_str())))?
            .read_to_end(&mut text_content)
            .await
            .map_err(|e| create_err(Some(e.to_string().as_str())))?;

        content.push_str(std::str::from_utf8(&text_content).unwrap());
    } else if mt.is_bmp() || mt.is_jpeg() || mt.is_png() || mt.is_gif() {
        post_type = PostType::Photo;

        let orig_path = upload.file.path().unwrap().to_string_lossy().to_string();
        let dest_filename = format!("{}.jpg", file_uuid);
        content.push_str(format!("/assets/{}", &dest_filename).as_str());

        // let orig_file = File::open(orig_path).map_err(|e| create_err(Some(e.to_string().as_str())))?;
        let orig_file = tokio::fs::read(orig_path).await.map_err(|e| create_err(Some(e.to_string().as_str())))?;

        // let file_reader = BufReader::new(orig_file);
        let read_buffer = Cursor::new(orig_file);

        // let mut image: DynamicImage = ImageReader::new(file_reader)
        //     .with_guessed_format()
        //     .map_err(|e| create_err(Some(e.to_string().as_str())))?
        //     .decode()
        //     .map_err(
        //         |e| {
        //             println!("Failed to decode image: {}", e);
        //
        //             create_err(Some(e.to_string().as_str()))
        //         }
        //     )?;
        let encoded_result: Result<DynamicImage, ()> = tokio::task::spawn_blocking(|| {
            Ok(ImageReader::new(read_buffer)
                .with_guessed_format()
                .map_err(|_| ())?
                .decode()
                .map_err(|_| ())?)
        })
            .await
            .map_err(|e| create_err(Some(e.to_string().as_str())))?;

        let mut image = encoded_result.map_err(|_| create_err(Some("something err on encoded")))?;

        // let dest_path = Path::new(rocket::fs::relative!("static")).join(&dest_filename);
        // let mut file_writer = File::create(dest_path).map_err(|e| create_err(Some(e.to_string().as_str())))?;
        // let encoder = JpegEncoder::new_with_quality(&mut file_writer, 75);
        //
        //
        if mt.is_png() {
            image = DynamicImage::from(image.into_rgb8());
        }

        // encoder.write_image(
        //         image.as_bytes(),
        //         image.width(),
        //         image.height(),
        //         ExtendedColorType::from(image.color()),
        //     )
        //     .map_err(
        //         |e| {
        //             println!("Failed to encode image: {}", e);
        //
        //             create_err(Some(e.to_string().as_str()))
        //         }
        //     )?;
        let write_result: Result<Vec<u8>, ImageError> = tokio::task::spawn_blocking(move || {
            let mut write_buffer: Vec<u8> = vec![];
            let mut write_cursor = Cursor::new(&mut write_buffer);
            let _ = JpegEncoder::new_with_quality(&mut write_cursor, 75).write_image(
                image.as_bytes(),
                image.width(),
                image.height(),
                ExtendedColorType::from(image.color()),
            )?;

            Ok(write_buffer)
        })
            .await
            .map_err(|e| create_err(Some(e.to_string().as_str())))?;
        let write_bytes = write_result.map_err(|e| create_err(Some(e.to_string().as_str())))?;
        let des_path = Path::new(rocket::fs::relative!("static")).join(&dest_filename);

        tokio::fs::write(&des_path, &write_bytes).await.map_err(|e| create_err(Some(e.to_string().as_str())))?;
    } else if mt.is_svg() {
        post_type = PostType::Photo;

        let dest_filename = format!("{}.svg", file_uuid);
        content.push_str(format!("/assets/{}", &dest_filename).as_str());
        let dest_path = Path::new(rocket::fs::relative!("static")).join(&dest_filename);

        upload.file.move_copy_to(&dest_path).await.map_err(|e| create_err(Some(e.to_string().as_str())))?;
    } else if mt.is_mp4() || mt.is_mpeg() || mt.is_ogg() || mt.is_mov() || mt.is_webm() {
        post_type = PostType::Video;
        let dest_filename = format!("{}.mp4", file_uuid);
        content.push_str("loading/assets/");
        content.push_str(&dest_filename);
        is_video = true;
        wm.orig_file_name = upload
            .file
            .path()
            .unwrap()
            .to_string_lossy()
            .to_string()
            .clone();
        wm.orig_file_name = dest_filename.clone();
    } else {
        return Err(create_err(None));
    }

    // Post::create(pool, user_uuid, post_type, &content)
    //     .await
    //     .map_err(|e| create_err(Some(e.to_string().as_str())))?;

    // Ok(Flash::success(
    //     Redirect::to(format!("/users/{}/posts", user_uuid)),
    //     "Successfully created a post.",
    // ))

    Ok(
        Post::create(pool, user_uuid, post_type, &content)
            .await
            .and_then(move |post| {
                if is_video {
                    wm.uuid = post.uuid.to_string();
                    let _ = tx.send(wm).map_err(|_| {
                        OurError::new_internal_server_error(
                            String::from("Cannot process message"),
                            None,
                        )
                    })?;
                }

                Ok(Flash::success(
                    Redirect::to(format!("/users/{}/posts", user_uuid)),
                    "Successfully created post",
                ))
            })
            .map_err(|e| create_err(Some(e.to_string().as_str())))?
    )
}

#[delete("/users/<_user_uuid>/posts/<_uuid>", format = "text/html")]
pub async fn delete_post(_pool: &rocket::State<SqlitePool>, _user_uuid: &str, _uuid: &str) -> HtmlResponse {
    todo!("will implement later");
}
