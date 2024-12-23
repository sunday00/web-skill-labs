use ch12_minigrep;
use std::process;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    // let query = &args[1];
    // let file_path = &args[2];
    let config = ch12_minigrep::Config::build(&args).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    // println!("Searching for {}", config.query);
    // println!("In file {}", config.file_path);

    if let Err(e) = ch12_minigrep::run(config) {
        eprintln!("Application error: {e}");
        process::exit(1);
    }
}

