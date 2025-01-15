use chrono::{DateTime, Utc};
use rocket::form;
use rocket::form::{DataField, FromFormField, ValueField};
use sqlx::FromRow;

#[derive(Debug, FromRow)]
pub struct OurDateTime(DateTime<Utc>);

#[rocket::async_trait]
impl<'r> FromFormField<'r> for OurDateTime {
    fn from_value(_: ValueField<'r>) -> form::Result<'r, Self> {
        todo!("will implement later");
    }

    async fn from_data(_: DataField<'r, '_>) -> form::Result<'r, Self> {
        todo!("will implement later");
    }
}

