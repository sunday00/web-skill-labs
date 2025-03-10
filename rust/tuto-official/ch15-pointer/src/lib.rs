use std::env;

mod a_box;
mod b_deref;
mod c_drop;
mod d_rc;
mod e_refcell;
mod f_circular;

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
        "rc" => {
            if args.get(2).unwrap().eq("1") {
                d_rc::ch1::exec();
            }
        }
        "refcell" => {
            if args.get(2).unwrap().eq("1") {
                e_refcell::ch1::exec();
            } else if args.get(2).unwrap().eq("2") {
                e_refcell::ch2::exec();
            }
        }
        "circular" => {
            if args.get(2).unwrap().eq("1") {
                f_circular::ch1::exec();
            } else if args.get(2).unwrap().eq("2") {
                f_circular::ch2::exec();
            }
        }
        _ => {}
    }
}
