struct Counter {}

pub trait Iterator {
    type Item;

    fn next(&mut self) -> Option<Self::Item>;
}

impl Iterator for Counter {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        // --생략--

        Option::Some(3)
    }
}

pub fn exec() {}
