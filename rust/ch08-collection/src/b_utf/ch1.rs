pub fn exec(_str: &String) {
    // let mut s = String::new();

    let data = "hello string";
    let _s = data.to_string();

    let mut s = String::from("who are you?");

    s.push_str(" bye?");

    s.push('k');
    s.push('t');
    s.push('L');

    println!("{}", s);

    let s1 = String::from("hello");
    let s2 = String::from("world");

    let s3 = s1 + &s2;
    // println!("{} {} {}",s1, s2, s3); // 🚫 s1 moved
    println!("{} {}", s2, s3);

    let s1 = String::from("hi");
    let s2 = String::from("he");
    let s3 = String::from("ho");

    println!("{}", format!("{}-{}-{}", s1, s2, s3));
    println!("{} {} {}", s1, s2, s3);
}