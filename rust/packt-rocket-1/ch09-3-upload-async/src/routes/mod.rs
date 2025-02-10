use rocket::fs::{relative, NamedFile};
use rocket::http::Status;
use rocket::Shutdown;
use rocket_dyn_templates::Template;
use std::path::{Path, PathBuf};

pub mod post;
pub mod user;

type HtmlResponse = Result<Template, Status>;

// ==== file upload ====
#[get("/<filename..>")]
pub async fn assets(filename: PathBuf) -> Option<NamedFile> {
    let mut filename = Path::new(relative!("static")).join(filename);
    if filename.is_dir() {
        filename.push("index.html");
    }
    NamedFile::open(filename).await.ok()
}

#[get("/shutdown")]
pub async fn shutdown(shutdown: Shutdown) -> &'static str {
    let result: Result<&str, &str> = Err("err");

    if result.is_err() {
        shutdown.notify();
        return "Shutting down";
    }

    "Nothing to do anything"
}
