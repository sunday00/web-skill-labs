use std::env;

pub mod a_vector;
pub mod b_utf;
pub mod c_hash;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "vector" => {
            if args.get(2).unwrap().eq("1") {
                a_vector::ch1::exec(args.get(3).unwrap())
            } else if args.get(2).unwrap().eq("2") {
                a_vector::ch2::exec(args.get(3).unwrap())
            } else if args.get(2).unwrap().eq("3") {
                a_vector::ch3::exec(args.get(3).unwrap())
            } else if args.get(2).unwrap().eq("4") {
                a_vector::ch4::exec(args.get(3).unwrap())
            }
        }
        "utf" => {
            if args.get(2).unwrap().eq("1") {
                b_utf::ch1::exec(args.get(3).unwrap())
            } else if args.get(2).unwrap().eq("2") {
                b_utf::ch2::exec(args.get(3).unwrap())
            }
        }
        "hash" => {
            if args.get(2).unwrap().eq("1") {
                c_hash::ch1::exec()
            } else if args.get(2).unwrap().eq("2") {
                c_hash::ch2::exec()
            }
        }
        _ => {}
    }
}
