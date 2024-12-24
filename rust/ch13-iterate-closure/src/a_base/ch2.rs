use std::thread;

pub fn exec() {
    let example_closure = |x| x;

    let _s = example_closure(String::from("hello"));
    // let n = example_closure(5); 🚫 closure infer already argument is String.

    let mut list = vec![1, 2, 3];
    list.iter().for_each(|x| println!("{}", x)); // read only borrow ref closure

    let mut mutate = || list.push(4);

    // println!("{:?}", list); // 🚫list is now mutable borrowed as define closure. but in print, currently list is immutable yet.

    mutate();

    println!("{:?}", list);

    thread::spawn(move || {
        println!("{:?}", list);
    }).join().unwrap(); // move to other thread
}
