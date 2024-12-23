use std::env;

pub mod a_base;
pub mod b_control;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "base" => {
            if args.get(2).unwrap().eq("1") {
                a_base::ch1::exec()
            }
        }
        "control" => {
            if args.get(2).unwrap().eq("1") {
                b_control::ch1::exec()
            }
        }
        _ => {}
    }
}