use argon2::password_hash::SaltString;
use argon2::{Argon2, PasswordHash, PasswordHasher, PasswordVerifier};
use rand_core::{OsRng, RngCore};
use rocket::fairing::{Fairing, Info, Kind};
use rocket::http::{Cookie, Status};
use rocket::request::{FromRequest, Outcome};
use rocket::serde::Serialize;
use rocket::{Build, Request, Rocket};
use time::{Duration, OffsetDateTime};

const CSRF_NAME: &str = "csrf_cookie";
const CSRF_LENGTH: usize = 32;
const CSRF_DURATION: Duration = Duration::hours(1);

trait RequestCsrf {
    fn get_csrf_token(&self) -> Option<Vec<u8>>;
}

impl RequestCsrf for Request<'_> {
    fn get_csrf_token(&self) -> Option<Vec<u8>> {
        self.cookies()
            .get_private(CSRF_NAME)
            .and_then(|c| base64::decode(c.value()).ok())
            .and_then(|r| {
                if r.len() >= CSRF_LENGTH {
                    Some(r)
                } else {
                    None
                }
            })
    }
}

#[derive(Debug, Clone)]
pub struct Csrf {}

impl Csrf {
    pub fn new() -> Self {
        Self {}
    }
}

#[rocket::async_trait]
impl Fairing for Csrf {
    fn info(&self) -> Info {
        Info {
            name: "CSRF Fairing",
            kind: Kind::Ignite | Kind::Request,
        }
    }

    async fn on_ignite(&self, rocket: Rocket<Build>) -> rocket::fairing::Result {
        Ok(rocket.manage(self.clone()))
    }

    async fn on_request(&self, req: &mut Request<'_>, _data: &mut rocket::Data<'_>) {
        if let Some(_) = req.get_csrf_token() {
            return;
        }
        let mut key = vec![0; CSRF_LENGTH];
        OsRng.fill_bytes(&mut key);
        let encoded = base64::encode(&key[..]);
        let expires = OffsetDateTime::now_utc() + CSRF_DURATION;
        let mut csrf_cookie = Cookie::new(String::from(CSRF_NAME), encoded);
        csrf_cookie.set_expires(expires);
        req.cookies().add_private(csrf_cookie);
    }
}


#[derive(Debug, Serialize)]
pub struct Token(String);

#[rocket::async_trait]
impl<'r> FromRequest<'r> for Token {
    type Error = ();

    async fn from_request(request: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        match request.get_csrf_token() {
            Some(csrf_token) => Outcome::Success(Self(base64::encode(csrf_token))),
            None => Outcome::Error((Status::Forbidden, ())),
        }
    }
}

impl Token {
    pub fn generate_hash(&self) -> Result<String, String> {
        let salt = SaltString::generate(&mut OsRng);
        Argon2::default()
            .hash_password(self.0.as_bytes(), &salt)
            .map(|hp| hp.to_string())
            .map_err(|_| String::from("cannot hash authenticity token"))
    }

    pub fn verify(&self, form_authenticity_token: &str) -> Result<(), String> {
        let old_password_hash = self.generate_hash()?;
        let parsed_hash = PasswordHash::new(&old_password_hash)
            .map_err(|_| String::from("cannot verify authenticity token"))?;

        Ok(
            Argon2::default()
                .verify_password(form_authenticity_token.as_bytes(), &parsed_hash)
                .map_err(|_| String::from("cannot verify authenticity token"))?
        )
    }
}


