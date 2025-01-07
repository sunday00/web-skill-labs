pub mod encryptor;

use std::io;
use encryptor::{rot13, Encryptable};

fn main () {
    println!("encryptor");

    let mut user_input = String::new();

    io::stdin().read_line(&mut user_input)
        .expect("Failed to read line");

    println!("input: {}", user_input);

    let t = rot13::Rot13(user_input);

    println!("result: {}", t.encrypt());
}