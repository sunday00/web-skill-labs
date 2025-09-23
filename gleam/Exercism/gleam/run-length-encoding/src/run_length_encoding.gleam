import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn encode(plaintext: String) -> String {
  en_reducer(
    "",
    plaintext |> string.first |> result.unwrap(""),
    0,
    plaintext |> string.to_graphemes,
  )
}

fn en_reducer(acc: String, cur: String, rp: Int, rem: List(String)) {
  case rem {
    [] -> {
      case rp {
        rp if rp == 0 -> ""
        rp if rp == 1 -> acc <> cur
        _ -> acc <> { rp |> int.to_string } <> cur
      }
    }
    [f, ..r] -> {
      case f == cur {
        True -> en_reducer(acc, cur, rp + 1, rem |> list.drop(1))
        False -> {
          case rp {
            rp if rp == 1 -> en_reducer(acc <> cur, f, 1, r)
            _ -> en_reducer(acc <> { rp |> int.to_string } <> cur, f, 1, r)
          }
        }
      }
    }
  }
}

pub fn decode(ciphertext: String) -> String {
  de_reducer("", "", ciphertext |> string.to_graphemes)
}

fn de_reducer(acc: String, part: String, rem: List(String)) {
  case rem {
    [] -> acc
    [f, ..r] -> {
      case int.parse(f) {
        Ok(p) -> de_reducer(acc, part <> f, r)
        _ -> {
          let repeater = part |> int.parse |> result.unwrap(1)
          de_reducer(acc <> { f |> string.repeat(repeater) }, "", r)
        }
      }
    }
  }
}

pub fn main() {
  echo decode("12WB12W3B24WB")
}
