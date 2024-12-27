pub fn exec() {
    let n = Option::from("x");

    let Some(x) = n
    else {
        print!("something went wrong");
        return;
    };

    println!("{}", x);

    if let Some(x) = n {
        println!("{}", x);
    }
}