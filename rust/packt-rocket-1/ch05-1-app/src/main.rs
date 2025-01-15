#[macro_use]
extern crate rocket;

use our_application::catchers;
use our_application::fairings::db::DBConnection;
use our_application::routes::assets;
use our_application::routes::{post, user};
use rocket::serde::{Deserialize, Serialize};
use rocket::{Build, Rocket};
use rocket_okapi::swagger_ui::{make_swagger_ui, SwaggerUIConfig};
// ==== [[[[routes]]]] ===========
// |
// |
// ==== user routes ====


// ==== post routes ====


// |
// |
// ==== [[[[routes]]]] ===========

// ==== [[[[define errors]]]] ===========
// |
// |

// |
// |
// ==== [[[[define errors]]]] ===========

// ==== [[[[config and launch]]]] ===========
// |
// |
#[derive(Serialize, Deserialize, Debug)]
pub struct Config {
    pub database_url: String,
}

#[launch]
async fn rocket() -> Rocket<Build> {
    let server = rocket::build();

    let config: Config = server
        .figment()
        .extract()
        .expect("Incorrect Rocket.toml configuration");

    let db = DBConnection::new(&config.database_url);

    server.manage(db.pool().await)
        // .mount("/", openapi_get_routes![])
        .mount("/", routes![
            user::get_user, user::get_users, user::new_user, user::edit_user,
            user::create_user,  user::put_user, user::patch_user, user::delete_user,
            post::get_post, post::get_posts, post::create_post, post::delete_post,
        ])
        .mount("/assets", routes![assets])
        .mount("/docs/", make_swagger_ui(&SwaggerUIConfig {
            url: "../openapi.json".to_owned(),
            ..Default::default()
        }), )
        .register("/", catchers![catchers::not_found, catchers::unprocessable_entity, catchers::internal_server_error])
}
// |
// |
// ==== [[[[config and launch]]]] ===========