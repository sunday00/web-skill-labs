import gleam/list
import gleam/result

fn t(n: Int) {
  use a <- list.flat_map(list.range(0, n))

  [a + 1]
}

fn t1() {
  [1, 2, 3] |> list.flat_map(fn(n) { [n + 1] })
}

fn t2() {
  use a <- list.map(list.range(1, 10))

  a + 1
}

fn t3() {
  use a <- list.map(list.range(1, 10))
  use b <- list.map(list.range(a + 1, 10 + a))

  b
}

fn inner_inner_t4(f: fn() -> String) {
  f()
}

fn inner_t4(i: Int, f: fn() -> String) {
  echo i
  use <- inner_inner_t4

  "Ok"
}

fn t4() {
  use <- inner_t4(3)

  "OK"
}

pub fn main() {
  todo
  // echo t(5)
  // echo t2()
  // echo t3()
  // echo t4()
}
