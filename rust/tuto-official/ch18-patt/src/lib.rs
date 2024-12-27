use std::env;

mod a_base;
mod b_failed;
mod c_syntax;
mod d_guard;
mod e_at;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let part = args.get(1).unwrap();

    match &part[..] {
        "base" => {
            if args.get(2).unwrap().eq("1") {
                a_base::ch1::exec()
            }
        }
        "failed" => {
            if args.get(2).unwrap().eq("1") {
                b_failed::ch1::exec()
            }
        }
        "syntax" => {
            if args.get(2).unwrap().eq("1") {
                c_syntax::ch1::exec()
            } else if args.get(2).unwrap().eq("2") {
                c_syntax::ch2::exec()
            } else if args.get(2).unwrap().eq("2-2") {
                c_syntax::ch2::exec2()
            } else if args.get(2).unwrap().eq("2-3") {
                c_syntax::ch2::exec3()
            } else if args.get(2).unwrap().eq("2-4") {
                c_syntax::ch2::exec4()
            } else if args.get(2).unwrap().eq("3") {
                c_syntax::ch3::exec()
            } else if args.get(2).unwrap().eq("3-2") {
                c_syntax::ch3::exec2()
            } else if args.get(2).unwrap().eq("4") {
                c_syntax::ch4::exec()
            }
        }
        "guard" => {
            if args.get(2).unwrap().eq("1") {
                d_guard::ch1::exec()
            }
        }
        "at" => {
            if args.get(2).unwrap().eq("1") {
                e_at::ch1::exec()
            }
        }
        _ => {}
    }
}