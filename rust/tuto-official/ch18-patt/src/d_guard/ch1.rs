pub fn exec() {
    let num = Some(4);

    match num {
        Some(x) if /* 👈 this is patt guard */ x % 2 == 0 => println!("The number {} is even", x),
        Some(x) => println!("The number {} is odd", x),
        None => (),
    }

    let x = Some(5);
    let y = 10;

    match x {
        Some(50) => println!("Got 50"),
        Some(n) if n == y/* 👈 this y is above y. */ => println!("Matched, n = {n}"),
        _ => println!("Default case, x = {:?}", x),
    }

    println!("at the end: x = {:?}, y = {y}", x);

    let x = 4;
    let y = false;

    match x {
        (4 | 5 | 6) if y => println!("yes"),
        _ => println!("no"),
    }
}