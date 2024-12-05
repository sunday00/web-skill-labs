use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let ch: u8 = args.get(1).unwrap().parse().unwrap();

    if ch == 1 {
        ch1(args.get(2).unwrap())
    } else if ch == 2 {
        // ch2(args.get(2).unwrap())
    } else if ch == 3 {
        // ch3(args.get(2).unwrap())
    }
}

#[derive(Debug)]
struct User {
    active: bool,
    username: String,
    email: String,
    sign_in_count: u64,
}

#[derive(Debug)]
struct Color(u8, u8, u8);

#[derive(Debug)]
struct AlwaysEqual;

#[derive(Debug)]
struct User2<'a> {
    active: bool,
    // username: &str, // 🤔lifetime need
    username: &'a str,
    email: String,
    sign_in_count: u64,
}

fn ch1(args: &str) {
    let mut user1 = User {
        active: true,
        username: String::from(args),
        email: String::from("wow@wow.com"),
        sign_in_count: 1,
    };

    user1.active = false;

    println!("{}", user1.username);

    let user2 = user_factory(String::from(args), String::from("wow2@wow.com"));

    println!("{:?}", user2);

    let user3 = User {
        username: String::from("socrates"),
        ..user1
    };

    println!("{}, {}", user3.email, user3.sign_in_count);
    // println!("{}", user1.email); // 🚫 ..user1 makes move to user3. user1 is not valid user.
    println!("{}", user1.username); // ✅ bun new user3 property, not from user1 is still valid.

    let white = Color(255, 255, 255);

    println!("{:?}", white);

    let subject = AlwaysEqual;
    println!("{:?}", subject);

    let user22 = User2 {
        username: "kobidu",
        email: String::from(args),
        active: true,
        sign_in_count: 3,
    };

    println!("{:?}", user22);
}

fn user_factory(username: String, email: String) -> User {
    User {
        active: true,
        username,
        email,
        sign_in_count: 1,
    }
}
