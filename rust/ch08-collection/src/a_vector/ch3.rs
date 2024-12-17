pub fn exec(_str: &String) {
    let mut v = vec![1, 2, 3];

    for i in &v {
        println!("{}", i);
    }

    for i in (&v).iter() {
        println!("{}", i);
    }

    for i in &mut v {
        *i = *i * 2;
    }

    println!("{:?}", v);
}