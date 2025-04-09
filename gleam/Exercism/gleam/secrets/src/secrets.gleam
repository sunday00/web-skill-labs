pub fn secret_add(secret: Int) -> fn(Int) -> Int {
  fn(n: Int) { n + secret }
}

pub fn secret_subtract(secret: Int) -> fn(Int) -> Int {
  fn(n: Int) { n - secret }
}

pub fn secret_multiply(secret: Int) -> fn(Int) -> Int {
  fn(n: Int) { n * secret }
}

pub fn secret_divide(secret: Int) -> fn(Int) -> Int {
  fn(n: Int) { n / secret }
}

pub fn secret_combine(
  secret_function1: fn(Int) -> Int,
  secret_function2: fn(Int) -> Int,
) -> fn(Int) -> Int {
  fn(n: Int) { secret_function1(n) |> secret_function2 }
}
