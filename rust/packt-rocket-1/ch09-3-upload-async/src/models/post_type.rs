use rocket::serde::Serialize;

#[derive(sqlx::Type, Debug, FromFormField, Serialize)]
#[repr(i32)]
pub enum PostType {
    Text = 0,
    Photo = 1,
    Video = 2,
}