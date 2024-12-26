use std::env;

mod a_base;
mod b_msg;
mod c_mutex;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "base" => {
            if args.get(2).unwrap().eq("1") {
                a_base::ch1::exec()
            } else if args.get(2).unwrap().eq("1-2") {
                a_base::ch1::exec2()
            } else if args.get(2).unwrap().eq("2") {
                a_base::ch2::exec()
            }
        }
        "msg" => {
            if args.get(2).unwrap().eq("1") {
                b_msg::ch1::exec()
            } else if args.get(2).unwrap().eq("2") {
                b_msg::ch2::exec()
            } else if args.get(2).unwrap().eq("2-2") {
                b_msg::ch2::exec2()
            }
        }
        "mutex" => {
            if args.get(2).unwrap().eq("1") {
                c_mutex::ch1::exec()
            } else if args.get(2).unwrap().eq("2") {
                c_mutex::ch2::exec()
            }
        }
        _ => {}
    }
}