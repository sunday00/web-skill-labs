pub fn exec() {
    // let r;
    //
    // {
    //     let x = 5;
    //     r = &x;
    // } // <------------- x is dead
    //
    // println!("r: {}", r); <----- r life > x life. x does not live ling enough.

    // let string1 = String::from("abcd");
    // let string2 = "xyz";
    //
    // let result = longest(string1.as_str(), string2);
    // println!("The longest string is {}", result);

    // let string1 = String::from("long string is long");
    // let result;
    // {
    //     let string2 = String::from("xyz");
    //     result = longest(string1.as_str(), string2.as_str());
    // }
    // println!("The longest string is {}", result);

    let string1 = String::from("long string is long");

    {
        let string2 = String::from("xyz");
        let result = longest(string1.as_str(), string2.as_str());
        println!("The longest string is {}", result);
    }
}

fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}