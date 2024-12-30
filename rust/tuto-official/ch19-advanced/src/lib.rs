use std::env;

mod a_unsafe;
mod b_trait;
mod c_type;
mod d_fn;

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
            } else if args.get(2).unwrap().eq("3") {
                b_trait::ch3::exec()
            } else if args.get(2).unwrap().eq("4") {
                b_trait::ch4::exec()
            } else if args.get(2).unwrap().eq("5") {
                b_trait::ch5::exec()
            } else if args.get(2).unwrap().eq("6") {
                b_trait::ch6::exec()
            }
        }
        "type" => {
            if args.get(2).unwrap().eq("1") {
                c_type::ch1::exec()
            } else if args.get(2).unwrap().eq("2") {
                c_type::ch2::exec()
            }
        }
        "fn" => {
            if args.get(2).unwrap().eq("1") {
                d_fn::ch1::exec()
            }
        }
        _ => {}
    }
}
