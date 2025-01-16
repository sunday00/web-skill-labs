use rocket::http::Status;
use sqlx::Error as sqlxError;
use std::borrow::Cow;
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug)]
pub struct OurError {
    pub status: Status,
    pub message: String,
    debug: Option<Box<dyn Error>>,
}

impl fmt::Display for OurError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for OurError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        if self.debug.is_some() {
            self.debug.as_ref().unwrap().source();
        }

        None
    }
}

impl OurError {
    fn new_error_with_status(status: Status, message: String, debug: Option<Box<dyn Error>>) -> Self {
        if debug.is_some() {
            log::error!("Error: {:?}", &debug);
        }
        OurError {
            status,
            message,
            debug,
        }
    }

    pub fn new_bad_request_error(message: String, debug: Option<Box<dyn Error>>) -> Self {
        Self::new_error_with_status(Status::BadRequest, message, debug)
    }

    pub fn new_not_found_error(message: String, debug: Option<Box<dyn Error>>) -> Self {
        Self::new_error_with_status(Status::NotFound, message, debug)
    }

    pub fn new_internal_server_error(message: String, debug: Option<Box<dyn Error>>) -> Self {
        Self::new_error_with_status(Status::InternalServerError, message, debug)
    }

    pub fn from_sqlx_error(e: sqlxError) -> Self {
        match e {
            sqlxError::RowNotFound => OurError::new_not_found_error(
                String::from("NotFound"), Some(Box::new(e)),
            ),
            sqlxError::Database(ee) => {
                if ee.code().unwrap_or(Cow::Borrowed("2067")).eq("2067") {
                    return OurError::new_bad_request_error(
                        String::from("Duplicated UniqueValue"), Some(Box::new(ee)),
                    );
                }

                if ee.code().unwrap_or(Cow::Borrowed("2300")).starts_with("23") {
                    return OurError::new_bad_request_error(
                        String::from("Cannot create or update resource"), Some(Box::new(ee)),
                    );
                }

                OurError::new_internal_server_error(
                    String::from("Something went wrong"), Some(Box::new(ee)),
                )
            }
            _ => OurError::new_internal_server_error(
                String::from("Something went wrong"), Some(Box::new(e)),
            )
        }
    }
}