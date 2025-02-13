use chrono::{DateTime, TimeZone, Utc};
use rocket::data::ToByteUnit;
use rocket::form;
use rocket::form::{DataField, FromFormField, ValueField};
use sqlx::FromRow;
use std::str::from_utf8;

#[derive(Debug, FromRow, sqlx::Type, Clone)]
#[sqlx(transparent)]
pub struct OurDateTime(pub DateTime<Utc>);

#[rocket::async_trait]
impl<'r> FromFormField<'r> for OurDateTime {
    fn from_value(field: ValueField<'r>) -> form::Result<'r, Self> {
        let timestamp = field.value.parse::<i64>()?;
        Ok(OurDateTime(Utc.timestamp_nanos(timestamp)))
    }

    async fn from_data(field: DataField<'r, '_>) -> form::Result<'r, Self> {
        let limit = field.request.limits().get("forms").unwrap_or_else(|| 8.kibibytes());
        let bytes = field.data.open(limit).into_bytes().await?;

        if !bytes.is_complete() {
            return Err((None, Some(limit)).into());
        }

        let bytes = bytes.into_inner();
        let time_string = from_utf8(&bytes)?;
        let timestamp = time_string.parse::<i64>()?;

        Ok(OurDateTime(Utc.timestamp_nanos(timestamp)))
    }
}

