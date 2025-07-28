import gleam/int
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string

pub fn is_valid(isbn: String) -> Bool {
  case isbn {
    "" -> False
    _ -> {
      let assert Ok(patt) = regexp.from_string("[^0-9X-]")

      !regexp.check(patt, isbn)
      && isbn |> string.replace("-", "") |> string.length == 10
      && {
        isbn
        |> string.replace("-", "")
        |> string.to_graphemes
        |> list.index_map(fn(n, i) { #(i, n) })
        |> list.fold(0, reducer)
      }
      % 11
      == 0
    }
  }
}

fn reducer(acc: Int, cur: #(Int, String)) {
  let #(i, n) = cur
  let nn = case n {
    "X" -> {
      case i {
        9 -> 10
        _ -> -999
      }
    }
    _ -> n |> int.parse |> result.unwrap(0)
  }

  acc + { { 10 - i } * nn }
}
