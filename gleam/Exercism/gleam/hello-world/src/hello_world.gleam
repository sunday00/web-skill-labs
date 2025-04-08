import gleam/io

pub fn hello() {
  "Hello, World!"
}

pub fn main() {
  io.println(hello())
}