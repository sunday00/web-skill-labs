use super::our_date_time::OurDateTime;
use super::post_type::PostType;

#[derive(FromForm)]
pub struct Post {
    uuid: String,
    user_uuid: String,
    post_type: PostType,
    content: String,
    created_at: OurDateTime,
}