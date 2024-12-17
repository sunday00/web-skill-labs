pub fn exec(_str: &String) {
    let mut v1: Vec<u8> = Vec::new();
    let mut v2: Vec<u8> = vec![1, 2, 3];

    println!("test {:?} {:?}", v1, v2);

    v1.push(1);
    v1.push(2);
    v1.push(3);

    v2.push(4);
    v2.push(5);
    v2.push(6);

    println!("test {:?} {:?}", v1, v2);
    println!("test {:?} {:?}", &v1.get(0), &v1[1]);
}
