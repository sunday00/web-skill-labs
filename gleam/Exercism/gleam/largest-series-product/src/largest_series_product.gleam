import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn largest_product(digits: String, span: Int) -> Result(Int, Nil) {
  let digits_len = string.length(digits)

  case span {
    n if n == 0 -> Ok(1)
    n if n < 0 || n > digits_len -> {
      Error(Nil)
    }
    _ -> {
      case int.parse(digits) {
        Ok(_) -> {
          reducer(0, digits, span)
        }
        Error(_) -> {
          Error(Nil)
        }
      }
    }
  }
}

fn reducer(max: Int, remains: String, span: Int) {
  let chunk = remains |> string.slice(0, span)

  let new_max =
    chunk
    |> string.to_graphemes()
    |> list.map(fn(n) { n |> int.parse |> result.unwrap(0) })
    |> int.product
    |> int.max(max)

  let new_remains = remains |> string.drop_start(1)

  case new_remains |> string.length >= span {
    True -> reducer(new_max, new_remains, span)
    False -> {
      Ok(new_max)
    }
  }
}
// pub fn main() {
//   echo largest_product("0123456789", 3)
// }
