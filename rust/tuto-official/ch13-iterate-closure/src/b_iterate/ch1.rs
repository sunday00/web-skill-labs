pub fn exec() {
    very_default();

    what_is_next();

    mutable_iter();

    filter();
}

fn very_default() {
    let v1 = vec![1, 2, 3];

    let v1_iter = v1.iter();

    for val in v1_iter {
        println!("Got: {}", val);
    }
}

fn what_is_next() {
    let v1 = vec![1, 2, 3];

    let mut v1_iter = v1.iter();

    //
    // println!("{}", v1_iter.next().expect("ended"));
    // println!("{}", v1_iter.next().expect("ended"));
    // println!("{}", v1_iter.next().expect("ended"));
    // println!("{}", v1_iter.next().expect("ended"));
    // println!("{}", v1_iter.next().expect("ended"));

    loop {
        let n = v1_iter.next();
        match n {
            Some(x) => println!("Got: {}", x),
            None => break,
        }
    }

    let v1_iter = v1.iter();
    let tot: u8 = v1_iter.sum();

    println!("{}", tot);
}

fn mutable_iter() {
    let v1 = vec![1, 2, 3];

    let v1_iter = v1.iter();

    // let v2_iter = v1_iter.map(|x| x + 1);

    // for val in v2_iter {
    //     println!("Got: {}", val);
    // }

    let v2: Vec<_> = v1_iter.map(|x| x + 1).collect();
    println!("{:?}", v2);
}

#[derive(PartialEq, Debug)]
struct Shoe {
    size: u32,
    style: String,
}

fn filter() {
    let shoes = vec![
        Shoe {
            size: 10,
            style: String::from("sneaker"),
        },
        Shoe {
            size: 13,
            style: String::from("sandal"),
        },
        Shoe {
            size: 10,
            style: String::from("boot"),
        },
    ];

    let fit_shoes: Vec<Shoe> = shoes.into_iter().filter(|s| s.size == 10).collect();

    println!("fit_shoes = {:?}", fit_shoes);
}