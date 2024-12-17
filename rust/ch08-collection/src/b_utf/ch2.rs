pub fn exec(_str: &String) {
    let k = "안녕하세요?";
    // let fst = k[0]; // 🚫
    // rust 에서는 유니코드 vector 컬렉션으로 언어별 0번에 들어 있는 값이 다르다.
    // 다른 언어에서 처럼 첫 번째를 냅다 줄 수 있지 않다.
    // 1byte 를 차지 하는 언어에서는 1글자의 유니코드를 갖고 있지만,
    // 2byte 를 차지 하는 언어에서는 0번째가 그 반만 갖고 있다.

    let fst = &k[0..3];
    println!("{}", fst); // 한글은 1글자 뽑으려면 3byte 단위로

    let fst = &k[3..6];
    println!("{}", fst); // 한글은 1글자 뽑으려면 3byte 단위로

    for i in k.chars() {
        println!("{}", i);
    }

    println!("{}", k);

    for i in k.bytes() {
        print!("{} | ", i);
    }

    println!("{}", "");

    for i in k.chars() {
        for y in i.encode_utf8(&mut [0; 3]).bytes() {
            println!("{} | {}", i, y);
        }
    }
}