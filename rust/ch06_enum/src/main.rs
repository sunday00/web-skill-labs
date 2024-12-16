use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let ch: u8 = args.get(1).unwrap().parse().unwrap();

    if ch == 1 {
        ch1(args.get(2).unwrap())
    } else if ch == 2 {
        // ch2(args.get(2).unwrap(), args.get(3).unwrap())
    }
}

enum IpAddrKind {
    V4,
    V6,
}

fn ch1(arg: &str) {
    let f = IpAddrKind::V4;
    let s = IpAddrKind::V6;

    // route(f);

    let home = IpAddr {
        kind: IpAddrKind::V4,
        address: String::from("127.0.0.1"),
    };

    let loopback = IpAddr {
        kind: IpAddrKind::V6,
        address: String::from("::1"),
    }
}

struct IpAddr {
    kind: IpAddrKind,
    address: String,
}

// fn route(ip_kind: IpAddrKind) {
//     let home = IpAddr {
//         kind: IpAddrKind::V4,
//         address: String::from("127.0.0.1"),
//     };
// }