use std::fmt;
use std::process::abort;

type Kilometers = i32;

type Result<T> = std::result::Result<T, std::io::Error>;

pub trait Write {
    fn write(&mut self, buf: &[u8]) -> Result<usize>;
    fn flush(&mut self) -> Result<()>;

    fn write_all(&mut self, buf: &[u8]) -> Result<()>;
    fn write_fmt(&mut self, fmt: fmt::Arguments) -> Result<()>;
}

pub fn exec() {
    let i: i32 = 4;
    let k: Kilometers = 5;
    let v = vec![1, 2, 3, i, k];

    println!("{:?}", v);
}

fn bar() -> ! {
    // --생략--
    abort();
}

fn baz() -> ! {
    loop {
        // 
    }
}