import gleam/int
import gleam/list
import gleam/string

pub fn valid(value: String) -> Bool {
  case string.trim(value) == "0" {
    True -> False
    False -> {
      let l =
        value
        |> string.replace(" ", "")
        |> string.to_graphemes
        |> list.reverse

      let r = reducer(0, 1, l)

      case r % 10 {
        0 -> True
        _ -> False
      }
    }
  }
}

fn reducer(acc: Int, cur: Int, rem: List(String)) {
  case rem {
    [f, ..r] -> {
      case int.parse(f) {
        Ok(ff) -> {
          case cur {
            1 -> {
              reducer(acc + ff, 2, r)
            }
            _ -> {
              let i = case ff * 2 > 9 {
                True -> ff * 2 - 9
                False -> ff * 2
              }

              reducer(acc + i, 1, r)
            }
          }
        }
        _ -> -1
      }
    }
    [] -> acc
  }
}
