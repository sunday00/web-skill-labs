import gleam/dict.{type Dict}
import gleam/list
import gleam/regexp
import gleam/string

pub fn count_words(input: String) -> Dict(String, Int) {
  reducer(dict.new(), "", input |> string.lowercase |> string.to_graphemes)
}

fn reducer(acc: Dict(String, Int), cur: String, rem: List(String)) {
  let assert Ok(patt) = regexp.from_string("[a-z0-9]")

  case rem {
    [f, ..r] -> {
      case patt |> regexp.check(f) {
        True -> reducer(acc, cur <> f, r)
        False -> {
          case f {
            "'" -> {
              case check_is_aps(cur, r) {
                True -> reducer(acc, cur <> f, r)
                False -> reducer(acc, cur, r |> list.prepend(" "))
              }
            }
            _ -> {
              case string.length(cur) > 0 {
                False -> reducer(acc, cur, r)
                True -> {
                  case acc |> dict.get(cur) {
                    Ok(d) -> reducer(acc |> dict.insert(cur, d + 1), "", r)
                    _ -> reducer(acc |> dict.insert(cur, 1), "", r)
                  }
                }
              }
            }
          }
        }
      }
    }
    [] -> {
      case string.length(cur) > 0 {
        True -> {
          case acc |> dict.get(cur) {
            Ok(d) -> acc |> dict.insert(cur, d + 1)
            _ -> acc |> dict.insert(cur, 1)
          }
        }
        False -> acc
      }
    }
  }
}

fn check_is_aps(cur: String, rem: List(String)) {
  case rem |> list.first {
    Ok(r_f) -> {
      list.contains(["that's", "don't", "you'r", "can't"], cur <> "'" <> r_f)
    }
    _ -> False
  }
}
