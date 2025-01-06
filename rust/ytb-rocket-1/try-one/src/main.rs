#[macro_use]
extern crate rocket;

use reqwest::Client;
use rocket::futures::TryFutureExt;
use rocket::http::uri::Origin;
use rocket::http::Status;
use rocket::response::Redirect;
use rocket::serde::json::{json, Value};
use rocket::serde::{json::Json, Serialize};
use rocket::State;
use std::collections::HashMap;
use std::time::SystemTime;

const API: Origin<'static> = uri!("/api");
const REPO: &str = "sunday00/lecture-master";

pub struct TauriGHRelease {
    pub value: Value,
    pub expiry: SystemTime,
}

#[get("/")]
fn index() -> &'static str {
    "hello rocket"
}

pub fn remove_suffix<'a>(s: &'a str, suffix: &str) -> &'a str {
    s.strip_suffix(suffix).unwrap_or_else(|| s)
}

async fn create_tauri_response(raw: &Value) -> Option<Value> {
    let platforms_available: HashMap<&str, Vec<&str>> = HashMap::from([
        ("amd64.AppImage.tar.gz", vec!["/linux-x86_64"]),
        ("app.tar.gz", vec!["/darwin-x86_64"]),
        ("x64_en-US.msi.zip", vec!["/windows-x86_64"]),
    ]);

    let mut res = json!({
        "version": raw["tag_name"].as_str()?,
        "notes": remove_suffix(&raw["body"].as_str()?, "See the assets to download this version and install.").trim_end_matches(['\r', '\n', ' ']),
        "pbu_date": raw["published_at"].as_str()?,
        "platforms": {},
    });

    // mapping assets ...

    Some(res)
}

async fn get_latest_release(client: &Client, repo: &str) -> Result<Value, reqwest::Error> {
    let url = format!("https://api.github.com/repos/{}/releases/latest", REPO);
    let res = client.get(&url).send().await?;

    // let release: Value = res.json().await?;
    let release = res.json::<Value>().await?;

    create_tauri_response(&release).await.ok_or(json!({})).or_else(|e| Ok(e))

    // Ok(release)
}

#[get("/google-keep-desktop/<_platform>/<current_version>?<msg>")]
async fn google_keep_desktop(_platform: &str, current_version: &str, msg: Option<&str>, client: &State<Client>) -> Result<Value, Status> {
    if let Some(_msg) = msg {
        // return Status::Ok;
        return Err(Status::BadRequest);
    }

    get_latest_release(client, REPO).await.or(Err(Status::NoContent))

    // Status::Ok
    // Ok(json!({"msg": current_version}))
}

#[get("/go")]
fn redirect_to_home() -> Redirect {
    // Redirect::to(uri!("/gg/google-keep-desktop/mac/v1"))
    Redirect::to(uri!(API, google_keep_desktop("mac", "v1", None as Option<&str>)))
}

#[get("/j-e")]
fn json_example() -> Value {
    json!({ "status": "ok"})
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index, redirect_to_home])
        .manage(
            Client::builder()
                .user_agent("reqwest")
                .build()
                .unwrap()
        )
        .mount(API, routes![google_keep_desktop, json_example])
}