@external(javascript, "./test.mjs", "getHello")
pub fn get_hello() -> String

pub fn main() {
  echo get_hello()
}
