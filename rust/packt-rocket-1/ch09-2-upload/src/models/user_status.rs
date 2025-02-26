use rocket::serde::Serialize;
use std::fmt;
use std::fmt::Formatter;

#[derive(sqlx::Type, Debug, FromFormField, Serialize)]
#[repr(i32)]
pub enum UserStatus {
    Inactive = 0,
    Active = 1,
}

impl fmt::Display for UserStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            UserStatus::Inactive => write!(f, "Inactive"),
            UserStatus::Active => write!(f, "Active"),
        }
    }
}