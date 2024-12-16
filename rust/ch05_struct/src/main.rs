use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let ch: u8 = args.get(1).unwrap().parse().unwrap();

    if ch == 1 {
        ch1(args.get(2).unwrap())
    } else if ch == 2 {
        ch2(args.get(2).unwrap(), args.get(3).unwrap())
    } else if ch == 22 {
        ch22(args.get(2).unwrap(), args.get(3).unwrap())
    } else if ch == 23 {
        ch23(args.get(2).unwrap(), args.get(3).unwrap())
    } else if ch == 24 {
        ch24(args.get(2).unwrap(), args.get(3).unwrap())
    } else if ch == 3 {
        ch3(args.get(2).unwrap(), args.get(3).unwrap())
    } else if ch == 32 {
        ch32(args.get(2).unwrap(), args.get(3).unwrap())
    } else if ch == 33 {
        ch33(args.get(2).unwrap(), args.get(3).unwrap())
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

fn ch2(arg1: &str, arg2: &str) {
    let width1 = arg1.parse().unwrap();
    let height1 = arg2.parse().unwrap();

    println!(
        "The area of the rectangle is {} square pixels.",
        area(width1, height1)
    );
}

fn area(width: u32, height: u32) -> u32 {
    width * height
}

fn ch22(arg1: &str, arg2: &str) {
    let rect1 = (arg1.parse().unwrap(), arg2.parse().unwrap());

    println!(
        "The area of the rectangle is {} square pixels.",
        area2(rect1)
    );
}

fn area2(dimension: (u32, u32)) -> u32 {
    dimension.0 * dimension.1
}

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

fn ch23(arg1: &str, arg2: &str) {
    let rect1 = Rectangle {
        width: arg1.parse().unwrap(),
        height: arg2.parse().unwrap(),
    };

    println!(
        "The area of the rectangle is {} square pixels.",
        area3(&rect1)
    );

    println!("{:?}", rect1);
    println!("{:#?}", rect1);
}

fn area3(r: &Rectangle) -> u32 {
    r.width * r.height
}

fn ch24(arg1: &str, arg2: &str) {
    let scale = 3;
    let w: u32 = arg1.parse().unwrap();
    let rect1 = Rectangle {
        width: dbg!(w * scale),
        height: arg2.parse().unwrap(),
    };

    println!(
        "The area of the rectangle is {} square pixels.",
        area3(&rect1)
    );

    dbg!(&rect1);
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
    fn width(&self) -> bool {
        self.width > 0
    }
}

fn ch3(arg1: &str, arg2: &str) {
    let rect1 = Rectangle {
        width: arg1.parse().unwrap(),
        height: arg2.parse().unwrap(),
    };

    println!("square area:: {}", rect1.area());
    println!("square width is valid:: {}", rect1.width());
    println!("square width is valid:: {}", &rect1.width()); // Rust doesn't bother whether you called on reverse pointer or not.
                                                            // On c, c++,
                                                            // when you call method from Var, code like -> rect1.width(),
                                                            // when you call method from ref, code list -> &rect1->width()
                                                            // But rust can realize automatically borrowed or not. so, just code A.b()
}

impl Rectangle {
    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}

fn ch32(arg1: &str, arg2: &str) {
    let rect1 = Rectangle {
        width: arg1.parse().unwrap(),
        height: arg2.parse().unwrap(),
    };

    let rect2 = Rectangle {
        width: 30,
        height: 100,
    };

    let rect3 = Rectangle {
        width: 80,
        height: 70,
    };

    println!("rect1 can hold rect2 :: {}", rect1.can_hold(&rect2));
    println!("rect1 can hold rect2 :: {}", rect1.can_hold(&rect3));
}

impl Rectangle {
    fn square(size: u32) -> Self {
        Self {
            width: size,
            height: size,
        }
    }
    fn new(w: u32, h: u32) -> Self {
        // new is not keyword, but just customizable variable text.
        Self {
            width: w,
            height: h,
        }
    }
    fn twice(w: u32, h: u32) -> Self {
        Self {
            width: w * 2,
            height: h * 2,
        }
    } // not using self. this works like static method.
}

fn ch33(arg1: &str, arg2: &str) {
    let rect1 = Rectangle::twice(arg1.parse().unwrap(), arg2.parse().unwrap());
    // static method should work by called ::

    println!("square area:: {}", rect1.area());
}
