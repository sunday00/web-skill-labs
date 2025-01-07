pub mod encryptor;

use std::io;
use our_package::cipher::{rot13, Cipher, rsa};

fn main () {
    println!("encryptor");

    let mut user_input = String::new();

    io::stdin().read_line(&mut user_input)
        .expect("Failed to read line");

    println!("input: {}", user_input);

    // let t = rot13::Rot13(user_input);
    //
    // println!("result: {}", t.encrypted_string());

    let enc_input = rsa::Rsa::new(user_input).expect("Failed to create encryptor");
    let enc_str = enc_input.encrypted_string().expect("Failed to read encrypted string");
    println!("encrypted string: {}", enc_str);

    let dec_str = enc_input.original_string().expect("Failed to read decrypted string");
    println!("decrypted string: {}", dec_str);
}