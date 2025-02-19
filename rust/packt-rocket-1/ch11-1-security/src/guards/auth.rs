use rocket::http::Status;
use rocket::Request;
use rocket::request::{FromRequest, Outcome};
use rocket::serde::Serialize;
use sqlx::{Pool, Sqlite};
use sqlx::pool::MaybePoolConnection::Connection;
use crate::fairings::db::DBConnection;
use crate::models::user::User;

pub const LOGIN_COOKIE_NAME: &str = "user_uuid";

#[derive(Serialize)]
pub struct CurrentUser {
    pub user: User,
}

#[rocket::async_trait]
impl<'r> FromRequest<'r> for CurrentUser {
    type Error = ();

    async fn from_request(req: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        let error = Outcome::Error( (Status::Unauthorized, ()) );
        let parsed_cookie = req.cookies().get_private(LOGIN_COOKIE_NAME);
        if parsed_cookie.is_none() {
            return error;
        }
        let cookie = parsed_cookie.unwrap();
        let uuid = cookie.value();

        // TODO: this is morph to my pool...
        // TODO: originally, struct with connection. check and search more.
        // let parsed_db = req.guard::<Connection<DBConnection>>().await;
        let pool = req.guard::<&Pool<Sqlite>>().await.unwrap();
        let found_user = User::find_from_pool(pool, uuid);

        if found_user.is_err() {
            return error;
        }

        let user = found_user.unwrap();
        Outcome::Success(CurrentUser { user })
    }
}