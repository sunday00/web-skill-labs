use super::our_date_time::OurDateTime;
use super::user_status::UserStatus;
use crate::models::bool_wrapper::BoolWrapper;
use crate::models::clean_html;
use crate::models::pagination::{Pagination, DEFAULT_LIMIT};
use argon2::password_hash::SaltString;
use argon2::{Argon2, PasswordHasher};
use rand_core::OsRng;
use regex::Regex;
use rocket::form;
use rocket::form::Error as FormError;
use sqlx::{FromRow, SqlitePool};
use std::error::Error;
use uuid::Uuid;
use zxcvbn::zxcvbn;

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

    pub async fn create<'r>(pool: &rocket::State<SqlitePool>, new_user: &'r NewUser<'r>) -> Result<Self, Box<dyn Error>> {
        let uuid = Uuid::new_v4();
        let user_name = &(clean_html(new_user.username));
        let description = &(new_user.description.map(clean_html));

        let salt = SaltString::generate(&mut OsRng);
        let argon2 = Argon2::default();
        let password_hash = argon2.hash_password(new_user.password.as_bytes(), &salt);
        if password_hash.is_err() {
            return Err("cannot create password hash".into());
        }

        let query_str = r#"INSERT INTO users (uuid, username, email, password_hash, description, status) VALUES ($1, $2, $3, $4, $5, $6) RETURNING *"#;
        Ok(
            sqlx::query_as::<_, Self>(query_str)
                .bind::<&str>(uuid.to_string().as_ref())
                .bind(user_name)
                .bind(new_user.email)
                .bind(password_hash.unwrap().to_string())
                .bind(description)
                .bind(UserStatus::Inactive)
                .fetch_one(pool.inner()).await?
        )
    }
}

#[derive(Debug, FromForm)]
pub struct NewUser<'r> {
    #[field(validate = len(5..20).or_else(msg!("name length should be 5~20")))]
    pub username: &'r str,

    #[field(validate = validate_email().or_else(msg!("invalid email")))]
    pub email: &'r str,

    #[field(validate = validate_password().or_else(msg!("weak password")))]
    pub password: &'r str,

    #[field(validate = eq(self.password).or_else(msg!("pw_confirm should be equal to pw")))]
    pub password_confirmation: &'r str,

    #[field(default = "", /*name = uncased("html-field-name")*/)]
    pub description: Option<&'r str>,
}

fn validate_email(email: &str) -> form::Result<'_, ()> {
    const EMAIL_REGEX: &str = r#"(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])"#;
    let email_regex = Regex::new(EMAIL_REGEX).unwrap();
    if !email_regex.is_match(email) {
        return Err(FormError::validation("invalid email").into());
    }
    Ok(())
}

fn validate_password(password: &str) -> form::Result<'_, ()> {
    let entropy = zxcvbn(password, &[]);
    if (entropy.score() as i32) < 3 {
        return Err(FormError::validation("weak password").into());
    }

    Ok(())
}