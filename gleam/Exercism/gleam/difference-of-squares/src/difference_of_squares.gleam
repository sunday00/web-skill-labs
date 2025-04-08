import gleam/float
import gleam/int
import gleam/result

fn r_sum(acc: Int, cur: Int, fin: Int) -> Int {
  case cur == fin {
    True -> acc + cur
    _ -> r_sum(acc + cur, cur + 1, fin)
  }
}

pub fn square_of_sum(n: Int) -> Int {
  r_sum(0, 0, n)
  |> int.power(of: 2.0)
  |> result.unwrap(0.0)
  |> float.truncate
}

fn r_sq(acc: Float, cur: Int, fin: Int) -> Float {
  let r = acc +. { int.power(cur, 2.0) |> result.unwrap(0.0) }

  case cur == fin {
    True -> r
    _ -> r_sq(r, cur + 1, fin)
  }
}

pub fn sum_of_squares(n: Int) -> Int {
  r_sq(0.0, 0, n) |> float.truncate
}

pub fn difference(n: Int) -> Int {
  square_of_sum(n) - sum_of_squares(n)
}
