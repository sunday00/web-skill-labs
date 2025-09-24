import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn annotate(garden: String) -> String {
  let m =
    garden
    |> string.split("\n")
    |> list.index_map(fn(li, n) {
      li
      |> string.to_graphemes
      |> list.index_map(fn(ch, i) { #(#(i, n), ch) })
    })
    |> list.flatten

  garden
  |> string.split("\n")
  |> list.index_map(fn(li, n) {
    li
    |> string.to_graphemes
    |> list.index_map(fn(ch, i) {
      let key = #(i, n)
      case check_is_flower(key, m) {
        True -> ch
        False -> {
          let n = find_flower_count(key, m)
          case n {
            0 -> ch
            _ -> n |> int.to_string
          }
        }
      }
    })
    |> string.join("")
  })
  |> string.join("\n")
}

fn find_flower_count(cur: #(Int, Int), map: List(#(#(Int, Int), String))) {
  let tf_list = [
    check_is_flower(#(cur.0 - 1, cur.1 - 1), map),
    check_is_flower(#(cur.0, cur.1 - 1), map),
    check_is_flower(#(cur.0 + 1, cur.1 - 1), map),
    check_is_flower(#(cur.0 - 1, cur.1), map),
    check_is_flower(#(cur.0 + 1, cur.1), map),
    check_is_flower(#(cur.0 - 1, cur.1 + 1), map),
    check_is_flower(#(cur.0, cur.1 + 1), map),
    check_is_flower(#(cur.0 + 1, cur.1 + 1), map),
  ]

  tf_list |> list.filter(fn(el) { el == True }) |> list.length
}

fn check_is_flower(p: #(Int, Int), map: List(#(#(Int, Int), String))) {
  map |> list.key_find(p) |> result.unwrap("") == "*"
}
