use sqlx::sqlite::SqlitePoolOptions;
use sqlx::{Pool, Sqlite};

pub struct DBConnection<'r> {
    url: &'r str,
}

impl DBConnection<'_> {
    pub fn new(url: &str) -> DBConnection {
        DBConnection {
            url,
        }
    }

    pub async fn pool(&self) -> Pool<Sqlite> {
        SqlitePoolOptions::new()
            .max_connections(5)
            .connect(self.url)
            .await
            .expect("Unable to connect to database")
    }
}