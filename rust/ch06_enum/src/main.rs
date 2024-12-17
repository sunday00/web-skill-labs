use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let ch: u8 = args.get(1).unwrap().parse().unwrap();

    if ch == 1 {
        ch1(args.get(2).unwrap())
    } else if ch == 11 {
        ch11(args.get(2).unwrap())
    } else if ch == 12 {
        ch12(args.get(2).unwrap())
    } else if ch == 2 {
        ch2(args.get(2).unwrap(), args.get(3).unwrap())
    } else if ch == 22 {
        ch22(args.get(2).unwrap())
    } else if ch == 3 {
        ch3(args.get(2).unwrap())
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
    };
}

struct IpAddr {
    kind: IpAddrKind,
    address: String,
}

/**
// fn route(ip_kind: IpAddrKind) {
//     let home = IpAddr {
//         kind: IpAddrKind::V4,
//         address: String::from("127.0.0.1"),
//     };
// }
*/

enum IpAddr2 {
    // V4(String),
    // V6(String),
    V4(u8, u8, u8, u8),
    V6(String),
}


fn ch11(arg: &str) {
    // let home = IpAddr2::V4(String::from("127.0.0.1"));
    // let loopback = IpAddr2::V6(String::from("::1"));
    let home = IpAddr2::V4(127, 0, 0, 1);
    let loopback = IpAddr2::V6(String::from("::1"));
}

fn ch12(arg: &str) {
    #[derive(Debug)]
    enum Message {
        Quit,
        Write(String),
    }

    impl Message {
        fn call(&self) {
            println!("{:?}", self);
        }
    }

    let m = Message::Write(String::from("hello"));
    m.call();
}

fn ch2(arg: &str, arg2: &str) {
    #[derive(Debug)] // so we can inspect the state in a minute
    enum UsState {
        Alabama,
        Alaska,
        // --생략--
    }

    enum Coin {
        Penny,
        Nickel,
        Dime,
        Quarter(UsState),
    }

    fn value_in_cents(coin: Coin) -> u8 {
        match coin {
            Coin::Penny => 1,
            Coin::Nickel => 5,
            Coin::Dime => 10,
            Coin::Quarter(state) => {
                println!("State quarter from {:?}!", state);
                25
            }
        }
    }

    value_in_cents(Coin::Quarter(UsState::Alabama));
}

fn ch22(arg: &str) {
    fn plus_one(x: Option<i32>) -> Option<i32> {
        match x {
            None => None,
            Some(i) => Some(i + 1),
        }
    }

    println!("{:?}", plus_one(Some(3)));
    println!("{:?}", plus_one(None));
}

fn ch3(arg: &str) {
    let config_max: Option<u8> = Some(0u8);
    // match config_max {
    //     Some(max) => println!("The maximum is configured to be {}", max),
    //     _ => (),
    // }

    // if let Some(max) = config_max {
    //     println!("The maximum is configured to be {}", max);
    // }

    if let None = config_max {
        println!("The maximum is configured to be None");
    }
}