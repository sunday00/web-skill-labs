use std::env;

mod a_unsafe;
mod b_trait;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "unsafe" => {
            if args.get(2).unwrap().eq("1") {
                a_unsafe::ch1::exec()
            } else if args.get(2).unwrap().eq("2") {
                a_unsafe::ch2::exec()
            } else if args.get(2).unwrap().eq("2-2") {
                a_unsafe::ch2::exec2()
            } else if args.get(2).unwrap().eq("3") {
                a_unsafe::ch3::exec()
            } else if args.get(2).unwrap().eq("4") {
                a_unsafe::ch4::exec()
            } else if args.get(2).unwrap().eq("5") {
                a_unsafe::ch5::exec()
            }
        }
        "trait" => {
            if args.get(2).unwrap().eq("1") {
                b_trait::ch1::exec()
            } else if args.get(2).unwrap().eq("2") {
                b_trait::ch2::exec()
            } else if args.get(2).unwrap().eq("2-2") {
                b_trait::ch2::exec2()
            }
        }
        _ => {}
    }
}
