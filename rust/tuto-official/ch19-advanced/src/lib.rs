use std::env;

mod a_unsafe;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "unsafe" => {
            if args.get(2).unwrap().eq("1") {
                a_unsafe::ch1::exec()
            }
        }
        _ => {}
    }
}