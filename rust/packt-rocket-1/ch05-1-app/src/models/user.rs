use super::our_date_time::OurDateTime;
use super::user_status::UserStatus;
use crate::models::bool_wrapper::BoolWrapper;
use crate::models::pagination::{Pagination, DEFAULT_LIMIT};
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
<div><span>UUID: </span>{uuid}</div>
<div><span>Username: </span>{username}</div>
<div><span>Email: </span>{email}</div>
<div><span>Description: </span>{description}</div>
<div><span>Status: </span>{status}</div>
<div><span>Created At: </span>{created_at}</div>
<div><span>Updated At: </span>{updated_at}</div>
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

    pub fn to_mini_string(&self) -> String {
        format!(r#"
            <div><span>UUID: </span>{uuid} <span>Username: </span>{username}</div>
        "#,
                uuid = self.uuid,
                username = self.username,
        )
    }

    pub async fn find_all(pool: &rocket::State<SqlitePool>, pagination: Option<Pagination>) -> Result<(Vec<Self>, Option<Pagination>), Box<dyn Error>> {
        let pagination_prams: Pagination;

        if pagination.is_some() {
            pagination_prams = pagination.unwrap();
        } else {
            pagination_prams = Pagination {
                next: OurDateTime(chrono::offset::Utc::now()),
                limit: DEFAULT_LIMIT,
            };
        }

        let query_str = "SELECT * FROM users WHERE created_at < $1 ORDER BY created_at DESC LIMIT $2";

        let users = sqlx::query_as::<_, Self>(query_str)
            .bind(&pagination_prams.next)
            .bind(pagination_prams.limit as i32)
            .fetch_all(pool.inner())
            .await?;

        let mut new_pagination: Option<Pagination> = None;
        if users.len() == pagination_prams.limit {
            let query_str = "SELECT EXISTS(SELECT 1 FROM users WHERE created_at < $1 ORDER BY created_at DESC LIMIT 1)";
            let exists = sqlx::query_as::<_, BoolWrapper>(query_str)
                .bind(&users.last().unwrap().created_at)
                .fetch_one(pool.inner())
                .await?;
            if exists.0 {
                new_pagination = Some(Pagination {
                    next: users.last().unwrap().created_at.to_owned(),
                    limit: pagination_prams.limit,
                });
            }
        }
        Ok((users, new_pagination))
    }
}