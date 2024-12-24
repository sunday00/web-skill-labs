#[derive(Debug)]
#[allow(dead_code)]
enum Cell {
    Int(i32),
    Bool(bool),
    String(String),
    String2(&'static str),
}

pub fn exec(_str: &String) {
    let row = vec![
        Cell::Int(1),
        Cell::Bool(true),
        Cell::String(String::from("OK")),
        Cell::String2("fds")
    ];

    println!("{:?}", row);
}