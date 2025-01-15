use super::our_date_time::OurDateTime;
use super::user_status::UserStatus;
use sqlx::{FromRow, SqlitePool};
use std::error::Error;

#[derive(Debug, FromRow, FromForm)]
pub struct User {
    pub uuid: String,
    pub username: String,
    pub email: String,
    pub password_hash: String,
    pub description: Option<String>,
    pub status: UserStatus,
    pub created_at: OurDateTime,
    pub updated_at: OurDateTime,
}

impl User {
    pub async fn find(pool: &rocket::State<SqlitePool>, uuid: &str) -> Result<Self, Box<dyn Error>> {
        let query = "SELECT * FROM users WHERE uuid = $1";
        Ok(sqlx::query_as::<_, Self>(query).bind(uuid).fetch_one(pool.inner()).await?)
    }

    pub fn to_html_string(&self) -> String {
        format!(r#"
<div><span class="label">UUID: </span>{uuid}</div>
<div><span class="label">Username: </span>{username}</div>
<div><span class="label">Email: </span>{email}</div>
<div><span class="label">Description: </span>{description}</div>
<div><span class="label">Status: </span>{status}</div>
<div><span class="label">Created At: </span>{created_at}</div>
<div><span class="label">Updated At: </span>{updated_at}</div>
        "#,
                uuid = self.uuid,
                username = self.username,
                email = self.email,
                description = self.description.as_ref().unwrap_or(&String::from("")),
                status = self.status.to_string(),
                created_at = self.created_at.0.to_rfc3339(),
                updated_at = self.updated_at.0.to_rfc3339(),
        )
    }
}