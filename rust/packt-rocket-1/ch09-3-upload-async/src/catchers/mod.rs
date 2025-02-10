use rocket::response::content::RawHtml;
use rocket::Request;

#[catch(404)]
pub fn not_found(_: &Request) -> RawHtml<String> {
    todo!("will implement later");
}

#[catch(422)]
pub fn unprocessable_entity(_: &Request) -> RawHtml<String> {
    todo!("will implement later");
}

#[catch(500)]
pub fn internal_server_error(_: &Request) -> RawHtml<String> {
    todo!("will implement later");
}