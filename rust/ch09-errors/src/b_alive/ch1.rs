use std::fs::File;
use std::io::ErrorKind;

pub fn exec() {
    let greeting_file_result = File::open("hello.txt");

    // let greeting_file = match greeting_file_result {
    //     Ok(file) => file,
    //     // Err(err) => panic!("Error opening file: {}", err),
    //     Err(err) => match err.kind() {
    //         ErrorKind::NotFound => match File::create("hello.txt") {
    //             Ok(fc) => fc,
    //             Err(e) => panic!("Error creating file: {:?}", e),
    //         },
    //         other_error => panic!("Error opening file: {:?}", other_error),
    //     },
    // };

    // let greeting_file = greeting_file_result.unwrap_or_else(|err| match err.kind() {
    //     ErrorKind::NotFound => match File::create("hello.txt") {
    //         Ok(fc) => fc,
    //         Err(e) => panic!("Error creating file: {:?}", e),
    //     },
    //     other_error => panic!("Error opening file: {:?}", other_error),
    // });

    let greeting_file = greeting_file_result.unwrap_or_else(|error| {
        if error.kind() == ErrorKind::NotFound {
            File::create("hello.txt").unwrap_or_else(|error| {
                panic!("Error creating file: {:?}", error)
            })
        } else {
            panic!("Error opening file: {:?}", error)
        }
    });

    println!("{:?}", greeting_file.metadata());
}