import gleam/int
import gleam/result
import gleam/string

fn reducer(acc: String, cur: Int) {
  case cur {
    0 -> acc
    n if n >= 1000 -> {
      let count = int.floor_divide(n, 1000) |> result.unwrap(0)

      reducer(acc <> string.repeat("M", count), cur - { 1000 * count })
    }
    n if n < 1000 && n >= 900 -> {
      reducer(acc <> "CM", cur - 900)
    }
    n if n < 900 && n >= 500 -> {
      reducer(acc <> "D", cur - 500)
    }
    n if n < 500 && n >= 400 -> {
      reducer(acc <> "CD", cur - 400)
    }
    n if n < 400 && n >= 100 -> {
      let count = int.floor_divide(n, 100) |> result.unwrap(0)
      reducer(acc <> string.repeat("C", count), cur - { 100 * count })
    }
    n if n < 100 && n >= 90 -> {
      reducer(acc <> "XC", cur - 90)
    }
    n if n < 90 && n >= 50 -> {
      reducer(acc <> "L", cur - 50)
    }
    n if n < 50 && n >= 40 -> {
      reducer(acc <> "XL", cur - 40)
    }
    n if n < 40 && n >= 10 -> {
      let count = int.floor_divide(n, 10) |> result.unwrap(0)
      reducer(acc <> string.repeat("X", count), cur - { 10 * count })
    }
    n if n < 10 && n >= 9 -> {
      reducer(acc <> "IX", cur - 9)
    }
    n if n < 9 && n >= 5 -> {
      reducer(acc <> "V", cur - 5)
    }
    n if n < 5 && n >= 4 -> {
      reducer(acc <> "IV", cur - 4)
    }
    n if n < 4 && n >= 1 -> {
      reducer(acc <> string.repeat("I", n), cur - n)
    }
    _ -> ""
  }
}

pub fn convert(number: Int) -> String {
  reducer("", number)
}
// pub fn main() {
//   echo reducer("", 4)
// }
