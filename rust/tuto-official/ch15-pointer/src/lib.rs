use std::env;

pub mod a_box;
pub mod b_deref;
pub mod c_drop;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "box" => {
            if args.get(2).unwrap().eq("1") {
                a_box::ch1::exec();
            }
        }
        "deref" => {
            if args.get(2).unwrap().eq("1") {
                b_deref::ch1::exec();
            }
        }
        "drop" => {
            if args.get(2).unwrap().eq("1") {
                c_drop::ch1::exec();
            } else if args.get(2).unwrap().eq("2") {
                c_drop::ch2::exec();
            }
        }
        _ => {}
    }
}
