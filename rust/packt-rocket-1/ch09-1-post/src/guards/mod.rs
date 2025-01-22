use rocket::request::{FromRequest, Outcome};
use rocket::Request;

pub struct RawContentType<'r>(pub &'r str);

#[rocket::async_trait]
impl<'r> FromRequest<'r> for RawContentType<'r> {
    type Error = ();

    async fn from_request(request: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        let headers = request.headers().get_one("Content-Type").or(Some("")).unwrap();
        Outcome::Success(RawContentType(headers))
    }
}