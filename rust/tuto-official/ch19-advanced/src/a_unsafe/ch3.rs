pub fn exec() {
    unsafe {
        println!("abs -3 from c : {}", abs(-3));
    }
}

extern "C" {
    fn abs(input: i32) -> i32;
}
