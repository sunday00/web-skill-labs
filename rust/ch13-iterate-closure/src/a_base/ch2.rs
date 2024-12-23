pub fn exec() {
    let example_closure = |x| x;

    let s = example_closure(String::from("hello"));
    // let n = example_closure(5); 🚫 closure infer already argument is String.
}
