#[macro_use]
extern crate rocket;

use our_application::fairings::csrf::Csrf;
use our_application::fairings::db::DBConnection;
use our_application::models::worker::Message;
use our_application::routes::assets;
use our_application::routes::{post, user};
use our_application::workers::video::process_video;
use our_application::{catchers, routes};
use rocket::serde::{Deserialize, Serialize};
use rocket::{Build, Rocket};
use rocket_dyn_templates::Template;
use rocket_okapi::swagger_ui::{make_swagger_ui, SwaggerUIConfig};
use tokio::runtime::Handle;

#[derive(Serialize, Deserialize, Debug)]
pub struct Config {
    pub database_url: String,
}


fn setup_logger() {
    let (level, logger) = fern::Dispatch::new()
        .format(move |out, message, record| {
            out.finish(
                format_args!(
                    "[{date}] [{level}][{target}] [{message}]",
                    date = chrono::Local::now().format("[%Y-%m-%d][%H:%M:%S%.3f]"),
                    target = record.target(),
                    level = record.level(),
                    message = message,
                )
            )
        })
        .level(log::LevelFilter::Info)
        .chain(std::io::stdout())
        .chain(
            fern::log_file("logs/application.log")
                .unwrap_or_else(|_| panic!("Could not open log file")),
        )
        .into_log();
    async_log::Logger::wrap(logger, || 0).start(level).unwrap();
}

#[launch]
async fn rocket() -> Rocket<Build> {
    setup_logger();

    let (tx, rx) = flume::bounded::<Message>(5);

    let server = rocket::build();

    let config: Config = server
        .figment()
        .extract()
        .expect("Incorrect Rocket.toml configuration");

    let db = DBConnection::new(&config.database_url);
    let pool = db.pool().await;

    let rk = server.manage(pool.clone())
        .mount("/", routes![
            user::get_user, user::get_users, user::new_user,  user::create_user,
            user::edit_user, user::update_user, user::put_user, user::patch_user,
            user::delete_user_entry_point, user::delete_user,
            post::get_post, post::get_posts, post::create_post, post::delete_post,
            routes::shutdown,
        ])
        .mount("/assets", routes![assets])
        .mount("/docs/", make_swagger_ui(&SwaggerUIConfig {
            url: "../openapi.json".to_owned(),
            ..Default::default()
        }), )
        .attach(Template::fairing())
        .attach(Csrf::new())

        .manage(tx)

        .register("/", catchers![catchers::not_found, catchers::unprocessable_entity, catchers::internal_server_error]);

    tokio::task::spawn_blocking(move || loop {
        let wm = rx.recv().unwrap();

        let _handle = Handle::current();

        let _ = process_video(&pool, wm);
    });

    rk
}
