import gleam/int
import gleam/io

fn cnt_more_than_n(acc: Int, vs: List(Int), t: Int) -> Int {
  case vs {
    [] -> acc
    [f, ..r] -> {
      case f >= t {
        True -> cnt_more_than_n(acc + 1, r, t)
        False -> cnt_more_than_n(acc, r, t)
      }
    }
  }
}

pub fn main() {
  let l = [1, 2, 7, 8, 3, 4, 9]

  let c = cnt_more_than_n(0, l, 5)
  io.print(c |> int.to_string)
}
