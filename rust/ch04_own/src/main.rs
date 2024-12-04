use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let ch: u8 = args.get(1).unwrap().parse().unwrap();

    if ch == 1 {
        ch1(args.get(2).unwrap())
    } else if ch == 2 {
        ch2(args.get(2).unwrap())
    } else if ch == 3 {
        ch3(args.get(2).unwrap())
    }
}

fn ch1 (str : &String) {
    let mut s = String::from(str);

    s.push_str(" : pushed this text");

    println!("{}", s);

    let x = 5;
    let y = x;

    println!("x = {}, y = {}", x, y);

    let x = String::from(str);
    let y = x;

    // println!("x = {}, y = {}", x, y); // 🚫 x is already moved.
    println!("x = {}, y = {}", '🚫', y);

    let s = String::from("ho");
    take_ownership(s);
    // println!("{}", s); // 🚫 s is already moved to function

    let x = 3;
    keep_ownership(x);
    println!("x = {}", x); // ✅ x is not moved. x just copied.

    {
        let mut s = String::from("hello");
        s = take_and_return(s); // function takes ownership, so s ownership moved. then, new ownership returned.

        println!("{}", s);
    }

    {
        let s = String::from("hello");
        let (s, len) = take_and_return_with_origin(s);
        println!("s, len = {}, {}", s, len);
    }
}

fn take_ownership(str : String) {
    println!("{}", str)
}

fn keep_ownership(n : i32) {
    println!("{}", n)
}

fn take_and_return (mut str: String) -> String {
    str.push_str(" Ho?");

    str
}

// 🤔 maybe too much complicated way. ownership can be more simply keep original.
fn take_and_return_with_origin(str : String) -> (String, u8) {
    let len: u8 = str.len() as u8;
    (str, len)
}

fn ch2 (str : &String) {
    let mut s = String::from(str);
    let len = take_reference(&s);

    println!("{}, {}", s, len); // ✅ s is alive

    mutate_reference(&mut s);
    println!("s: {}", s);

    let mut s = String::from(str);
    let r1 = &s;
    let r2 = &s; // ✅ allow: create read ref many times
    let r3 = &s; // ✅ allow: create read ref many times
    println!("{}, {}, {}", r1, r2, r3);

    let r4 = &mut s;
    println!("{}", r4); //✅ allow: create read but already used, and create mutable ref.

    // let r5 = &s;
    // let r6 = &mut s;  // 🚫 not allowed to make read/mutable ref on same time.
    // println!("{}, {}", r5, r6); // &mut ref can mutate original, so, it could make reading ref conflict.
    // so, this logic forbidden.

    // let r7 = &mut s; // 🚫 not allowed to make mutable ref more than once on same time.
    // let r8 = &mut s;
    // let r9 = &mut s;
    // println!("{}, {}, {}", r7, r9, r8);  // same reason, mutation makes other mut ref conflict.

    let no_v = dangle();
    println!("{}", no_v);
}

fn take_reference(str : &String) -> u8 { // using reference == Borrowing
    str.len() as u8
}

fn mutate_reference(str : &mut String) {
    str.push_str(" Ho");
}

// fn _dangle () -> &'static String {
fn dangle () -> String {
    let s = String::from("hello");

    // &s // 🚫do not use like this.
    // original s: String be dropped when exiting this function,
    // but returns reference. so, without OWNER, only left BORROW. this call "DANGLING ERROR"

    s // ✅so, if you need to return value that was made on local function,
    // should return itself, not ref(borrow).
}

fn ch3 (str : &String) {
    let s = String::from(str);
    let fw = first_word(&s);
    let fw2 = first_word(&s[6..]);
    let fw3 = first_word("something is wrong");

    // s.clear();

    println!("{} {} {}", fw, fw2, fw3);

    slices();
}

fn first_word(s : &str) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}

fn slices() {
    let a = [1, 2, 3, 4, 5];
    let slice = &a[1..3];

    assert_eq!(slice, &[2, 3]);
}