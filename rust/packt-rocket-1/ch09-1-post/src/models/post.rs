use super::our_date_time::OurDateTime;
use super::post_type::PostType;
use crate::errors::our_error::OurError;
use crate::models::bool_wrapper::BoolWrapper;
use crate::models::pagination::{Pagination, DEFAULT_LIMIT};
use crate::models::photo_post::PhotoPost;
use crate::models::text_post::TextPost;
use crate::models::video_post::VideoPost;
use crate::traits::DisplayPostContent;
use chrono::Utc;
use rocket::form::FromForm;
use rocket::serde::Serialize;
use sqlx::{FromRow, SqlitePool};

#[derive(Debug, FromRow, rocket::FromForm, Serialize)]
pub struct Post {
    pub uuid: String,
    pub user_uuid: String,
    pub post_type: PostType,
    pub content: String,
    pub created_at: OurDateTime,
}

impl Post {
    pub fn to_text(self) -> TextPost {
        TextPost(self)
    }

    pub fn to_photo(self) -> PhotoPost {
        PhotoPost(self)
    }

    pub fn to_video(self) -> VideoPost {
        VideoPost(self)
    }

    pub fn to_media(self) -> Box<dyn DisplayPostContent> {
        match self.post_type {
            PostType::Text => Box::new(self.to_text()),
            PostType::Photo => Box::new(self.to_photo()),
            PostType::Video => Box::new(self.to_video()),
        }
    }

    pub fn to_html(self) -> String {
        match self.post_type {
            PostType::Text => self.to_text().raw_html(),
            PostType::Photo => self.to_photo().raw_html(),
            PostType::Video => self.to_video().raw_html(),
        }
    }

    pub async fn find(pool: &rocket::State<SqlitePool>, uuid: &str) -> Result<Self, OurError> {
        let query = "SELECT * FROM posts WHERE uuid = $1";
        Ok(
            sqlx::query_as::<_, Self>(query)
                .bind(uuid)
                .fetch_one(pool.inner())
                .await
                .map_err(OurError::from_sqlx_error)?
        )
    }

    pub async fn find_all(pool: &rocket::State<SqlitePool>, user_uuid: &str, pagination: Option<Pagination>) -> Result<(Vec<Self>, Option<Pagination>), OurError> {
        let pagination_prams: Pagination;

        if pagination.is_some() {
            pagination_prams = pagination.unwrap();
        } else {
            pagination_prams = Pagination {
                next: OurDateTime(Utc::now()),
                limit: DEFAULT_LIMIT,
            };
        }

        let query_str = "SELECT * FROM posts WHERE user_uuid = $1 AND created_at < $2 ORDER BY created_at DESC LIMIT $3";

        let posts = sqlx::query_as::<_, Self>(query_str)
            .bind(user_uuid)
            .bind(&pagination_prams.next)
            .bind(pagination_prams.limit as i32)
            .fetch_all(pool.inner())
            .await
            .map_err(OurError::from_sqlx_error)?;

        let mut new_pagination: Option<Pagination> = None;
        if posts.len() == pagination_prams.limit {
            let query_str = "SELECT EXISTS(SELECT 1 FROM posts WHERE user_uuid = $1 AND created_at < $2 ORDER BY created_at DESC LIMIT 1)";
            let exists = sqlx::query_as::<_, BoolWrapper>(query_str)
                .bind(user_uuid)
                .bind(&posts.last().unwrap().created_at)
                .fetch_one(pool.inner())
                .await
                .map_err(OurError::from_sqlx_error)?;
            if exists.0 {
                new_pagination = Some(Pagination {
                    next: posts.last().unwrap().created_at.to_owned(),
                    limit: pagination_prams.limit,
                });
            }
        }
        Ok((posts, new_pagination))
    }
}