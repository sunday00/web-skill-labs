use crate::a_box::ch1::List::{Cons, Nil};

#[derive(Debug)]
enum List {
    // Cons(i32, List),
    Cons(i32, Box<List>),
    Nil,
}

pub fn exec() {
    let b = Box::new(5);
    println!("{}", b);

    // let list = Cons(1, Cons(2, Cons(3,  Nil)));
    let list = Cons(
        1, Box::new(Cons(
            2, Box::new(Cons(
                3, Box::new(Nil),
            )),
        )),
    );

    println!("{:?}", list);
    if let Cons(x, y) = list { // ☝️ way 1.
        println!("{} {:?}", x, y);

        let bv = *y;

        match bv { // ✌️ way 2.
            Cons(a, b) => {
                println!("{} {:?}", a, b);

                let bv = *b;

                match bv {
                    Cons(n, _) => {
                        println!("{}", n);
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
}
