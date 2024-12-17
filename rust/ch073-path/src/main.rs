use ch073_path::customer;
use rand::Rng;
// use std::{cmp::Ordering, io};
// use std::io::{self, Write};
// use std::collections::*;


fn main() {
    customer::eat_at_restaurant();
    customer::hosting::add_to_waitlist();

    let s = rand::thread_rng().gen_range(1..=100);
}
