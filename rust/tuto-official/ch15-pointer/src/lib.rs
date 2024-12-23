use std::env;

pub mod a_box;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "box" => {
            if args.get(2).unwrap().eq("1") {
                a_box::ch1::exec();
            }
        }
        _ => {}
    }
}
