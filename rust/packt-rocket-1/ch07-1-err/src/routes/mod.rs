use rocket::fs::NamedFile;
use rocket::http::Status;
use rocket::response::content::RawHtml;
use rocket::Shutdown;

pub mod post;
pub mod user;

type HtmlResponse = Result<RawHtml<String>, Status>;

// ==== file upload ====
#[get("/<_filename>")]
pub async fn assets(_filename: &str) -> NamedFile {
    todo!("will implement later");
}

#[get("/shutdown")]
pub async fn shutdown(shutdown: Shutdown) -> &'static str {
    let result: Result<&str, &str> = Err("err");

    if result.is_err() {
        // show shutdown message to client lastly,
        // then shutting down gracefully and completely.
        shutdown.notify();
        return "Shutting down";
    }

    "Nothing to do anything"
}