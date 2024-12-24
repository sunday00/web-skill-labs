//! # welcome. this is tutorial document
//! - hohoho
//! - huhuhu

use std::env;

pub mod a_base;

/// Run code by argument
///
/// # description
/// - this is run from main
///
/// ```
/// use ch14_cargo::run;
/// run();
/// ```
///
/// check `cargo doc --open`
pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "base" => {
            if args.get(2).unwrap().eq("1") {
                a_base::ch1::exec()
            }
        }
        _ => {}
    }
}