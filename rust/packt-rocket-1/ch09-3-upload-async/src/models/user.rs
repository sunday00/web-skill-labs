use super::our_date_time::OurDateTime;
use super::user_status::UserStatus;
use crate::errors::our_error::OurError;
use crate::models::bool_wrapper::BoolWrapper;
use crate::models::clean_html;
use crate::models::pagination::{Pagination, DEFAULT_LIMIT};
use argon2::password_hash::SaltString;
use argon2::{Argon2, PasswordHash, PasswordHasher, PasswordVerifier};
use chrono::Utc;
use rand_core::OsRng;
use regex::Regex;
use rocket::form;
use rocket::form::Error as FormError;
use rocket::serde::Serialize;
use sqlx::{FromRow, SqlitePool};
use uuid::Uuid;

#[derive(Debug, FromRow, FromForm, Serialize)]
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
    pub async fn find(pool: &rocket::State<SqlitePool>, uuid: &str) -> Result<Self, OurError> {
        let query = "SELECT * FROM users WHERE uuid = $1";
        Ok(
            sqlx::query_as::<_, Self>(query)
                .bind(uuid)
                .fetch_one(pool.inner())
                .await
                .map_err(OurError::from_sqlx_error)?
        )
    }

    pub async fn find_all(pool: &rocket::State<SqlitePool>, pagination: Option<Pagination>) -> Result<(Vec<Self>, Option<Pagination>), OurError> {
        let pagination_prams: Pagination;

        if pagination.is_some() {
            pagination_prams = pagination.unwrap();
        } else {
            pagination_prams = Pagination {
                next: OurDateTime(Utc::now()),
                limit: DEFAULT_LIMIT,
            };
        }

        let query_str = "SELECT * FROM users WHERE created_at < $1 ORDER BY created_at DESC LIMIT $2";

        let users = sqlx::query_as::<_, Self>(query_str)
            .bind(&pagination_prams.next)
            .bind(pagination_prams.limit as i32)
            .fetch_all(pool.inner())
            .await
            .map_err(OurError::from_sqlx_error)?;

        let mut new_pagination: Option<Pagination> = None;
        if users.len() == pagination_prams.limit {
            let query_str = "SELECT EXISTS(SELECT 1 FROM users WHERE created_at < $1 ORDER BY created_at DESC LIMIT 1)";
            let exists = sqlx::query_as::<_, BoolWrapper>(query_str)
                .bind(&users.last().unwrap().created_at)
                .fetch_one(pool.inner())
                .await
                .map_err(OurError::from_sqlx_error)?;
            if exists.0 {
                new_pagination = Some(Pagination {
                    next: users.last().unwrap().created_at.to_owned(),
                    limit: pagination_prams.limit,
                });
            }
        }
        Ok((users, new_pagination))
    }

    pub async fn create<'r>(pool: &rocket::State<SqlitePool>, new_user: &'r NewUser<'r>) -> Result<Self, OurError> {
        let uuid = Uuid::new_v4();
        let user_name = &(clean_html(new_user.username));
        let description = &(new_user.description.map(clean_html));

        let salt = SaltString::generate(&mut OsRng);
        let argon2 = Argon2::default();
        let password_hash = argon2.hash_password(new_user.password.as_bytes(), &salt)
            .map_err(|e| {
                OurError::new_internal_server_error(
                    String::from("cannot create password hash"),
                    Some(Box::new(e)),
                )
            });

        let query_str = r#"INSERT INTO users (uuid, username, email, password_hash, description, status) VALUES ($1, $2, $3, $4, $5, $6) RETURNING *"#;

        Ok(
            sqlx::query_as::<_, Self>(query_str)
                .bind::<&str>(uuid.to_string().as_ref())
                .bind(user_name)
                .bind(new_user.email)
                .bind(password_hash.unwrap().to_string())
                .bind(description)
                .bind(UserStatus::Inactive)
                .fetch_one(pool.inner()).await
                .map_err(OurError::from_sqlx_error)?
        )
    }

    pub async fn update<'r>(pool: &rocket::State<SqlitePool>, uuid: &'r str, user: &'r EditedUser<'r>) -> Result<Self, OurError> {
        let old_user = Self::find(pool, uuid).await?;

        let now = OurDateTime(Utc::now());
        let username = &(clean_html(user.username));
        let description = &(user.description.map(|desc| clean_html(desc)));

        let mut set_strings = vec![
            "username = $1",
            "email = $2",
            "description = $3",
            "updated_at = $4",
        ];
        let mut where_string = "$5";
        let mut password_string = String::new();
        let is_with_password = !user.old_password.is_empty();

        if is_with_password {
            let old_password_hash = PasswordHash::new(&old_user.password_hash)
                .map_err(|e| {
                    OurError::new_internal_server_error(
                        String::from("Input error"),
                        Some(Box::new(e)),
                    )
                })?;
            let argon2 = Argon2::default();
            argon2.verify_password(user.old_password.as_bytes(), &old_password_hash)
                .map_err(|e| {
                    OurError::new_internal_server_error(
                        String::from("Cannot confirm old password"),
                        Some(Box::new(e)),
                    )
                })?;
            let salt = SaltString::generate(&mut OsRng);
            let new_hash = argon2.hash_password(user.password.as_bytes(), &salt)
                .map_err(|e| {
                    OurError::new_internal_server_error(
                        String::from("Something went wrong"),
                        Some(Box::new(e)),
                    )
                })?;

            password_string.push_str(&new_hash.to_string().as_ref());
            set_strings.push("password_hash = $5");
            where_string = "$6";
        }

        let query_str = format!(
            r#"UPDATE users SET {} WHERE uuid = {} RETURNING *"#, set_strings.join(", "), where_string
        );

        let mut binded = sqlx::query_as::<_, Self>(&query_str)
            .bind(username)
            .bind(user.email)
            .bind(description)
            .bind(&now);

        if is_with_password {
            binded = binded.bind(password_string);
        }

        Ok(binded.bind(uuid).fetch_one(pool.inner()).await.map_err(OurError::from_sqlx_error)?)
    }

    pub async fn destroy(pool: &rocket::State<SqlitePool>, uuid: &str) -> Result<(), OurError> {
        let query_str = r#"DELETE FROM users WHERE uuid = $1"#;

        sqlx::query(query_str).bind(uuid).execute(pool.inner()).await.map_err(OurError::from_sqlx_error)?;

        Ok(())
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

    pub authenticity_token: &'r str,
}

#[derive(Debug, FromForm)]
pub struct EditedUser<'r> {
    #[field(name = "_METHOD")]
    pub method: &'r str,

    #[field(validate = len(5..20).or_else(msg!("name length should be 5~20")))]
    pub username: &'r str,

    #[field(validate = validate_email().or_else(msg!("invalid email")))]
    pub email: &'r str,

    pub old_password: &'r str,

    #[field(validate = skip_validate_password(self.old_password, self.password_confirmation))]
    pub password: &'r str,

    #[field(validate = eq(self.password).or_else(msg!("pw_confirm should be equal to pw")))]
    pub password_confirmation: &'r str,

    #[field(default = "", /*name = uncased("html-field-name")*/)]
    pub description: Option<&'r str>,

    pub authenticity_token: &'r str,
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
    if password.len() < 4 {
        return Err(FormError::validation("should over 4 letters").into());
    }

    Ok(())
}

fn skip_validate_password<'v>(password: &'v str, old_password: &'v str, password_confirmation: &'v str) -> form::Result<'v, ()> {
    if old_password.is_empty() {
        return Ok(());
    }

    validate_password(password)?;
    if password.ne(password_confirmation) {
        return Err(FormError::validation("password confirmation mismatch").into());
    }

    Ok(())
}

#[derive(Serialize)]
pub struct GetUser {
    pub user: User,
    pub flash: Option<String>,
}
