use std::env;

pub mod a_terminate;
pub mod b_alive;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "terminate" => {
            if args.get(2).unwrap().eq("1") {
                a_terminate::ch1::exec();
            }
        }
        "alive" => {
            if args.get(2).unwrap().eq("1") {
                b_alive::ch1::exec();
            } else if args.get(2).unwrap().eq("2") {
                b_alive::ch2::exec();
            }
        }
        _ => {}
    }
}