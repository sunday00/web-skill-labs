static mut COUNTER: u32 = 0;

// Work in single thread. but, remember. this way is too danger in multi threads.
pub fn exec() {
    add_to_count(3);

    unsafe {
        println!("COUNTER: {}", COUNTER);
    }
}

fn add_to_count(inc: u32) {
    unsafe {
        COUNTER += inc;
    }
}
