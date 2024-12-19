use std::env;

pub mod a_base;
pub mod b_trait;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "base" => {
            if args.get(2).unwrap().eq("1") {
                a_base::ch1::exec()
            } else if args.get(2).unwrap().eq("2") {
                a_base::ch2::exec()
            }
        }
        "trait" => {
            if args.get(2).unwrap().eq("1") {
                b_trait::ch1::exec()
            } else if args.get(2).unwrap().eq("2") {
                b_trait::ch2::exec()
            }
        }
        _ => {}
    }
}