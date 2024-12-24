use std::fs;
use std::fs::File;
use std::io::Read;

pub fn exec() {
    // let greeting_file = File::open("hello.txt").unwrap();
    // let greeting_file = File::open("hello.txt").expect("\n\nOOOOOOOPS!!\n\n\n");

    let username = read_username_from_file().expect("\n\nOOOOOOOPS!!\n\n\n");

    println!("Your username is {}\n", username);
}

fn read_username_from_file() -> Result<String, std::io::Error> {
    let username_file_result = File::open("hello.txt");

    let mut username_file = match username_file_result {
        Ok(file) => file,
        Err(e) => return Err(e),
    };

    let mut username = String::new();

    match username_file.read_to_string(&mut username) {
        Ok(_) => Ok(username),
        Err(e) => Err(e)
    }
}

fn read_username_from_file_short() -> Result<String, std::io::Error> {
    let mut username_file = File::open("hello.txt")?;

    let mut username = String::new();

    username_file.read_to_string(&mut username)?;

    Ok(username)
}

fn read_username_from_file_shorter() -> Result<String, std::io::Error> {
    let mut username = String::new();

    File::open("hello.txt")?.read_to_string(&mut username)?;

    Ok(username)
}

fn read_username_from_file_shortest() -> Result<String, std::io::Error> {
    fs::read_to_string("hello.txt")
}

fn last_char_of_first_line(text: &str) -> Option<char> {
    text.lines().next()?.chars().last()
}