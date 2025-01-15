use super::our_date_time::OurDateTime;
use super::user_status::UserStatus;
use sqlx::FromRow;

#[derive(Debug, FromRow, FromForm)]
pub struct User {
    uuid: String,
    username: String,
    email: String,
    password_hash: String,
    description: String,
    status: UserStatus,
    created_at: OurDateTime,
    updated_at: OurDateTime,
}