use std::collections::HashSet;

pub mod our_date_time;
pub mod pagination;
pub mod user_status;
pub mod user;
pub mod post_type;
pub mod post;
pub mod text_post;
pub mod photo_post;
pub mod video_post;
pub mod bool_wrapper;

pub fn clean_html(src: &str) -> String {
    ammonia::Builder::default()
        .tags(HashSet::new())
        .clean(src)
        .to_string()
}