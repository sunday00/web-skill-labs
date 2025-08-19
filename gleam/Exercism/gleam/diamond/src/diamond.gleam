import gleam/list
import gleam/result
import gleam/string

pub fn build(letter: String) -> String {
  case letter {
    "A" -> "A"
    _ -> {
      let abc = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      let abc_map =
        abc |> string.to_graphemes |> list.index_map(fn(x, i) { #(x, i) })
      let i_map =
        abc |> string.to_graphemes |> list.index_map(fn(x, i) { #(i, x) })
      let s_map =
        abc
        |> string.to_graphemes
        |> list.index_map(fn(x, i) { #(x, { i } * 2 - 1) })
        |> list.key_set("A", 0)

      let space = s_map |> list.key_find(letter) |> result.unwrap(0)
      let mid = letter <> string.repeat(" ", space) <> letter

      let target_i = abc_map |> list.key_find(letter) |> result.unwrap(0)

      let res = reducer([], target_i, 1, i_map, s_map)

      res
      |> list.append([mid])
      |> list.append(res |> list.reverse)
      |> string.join("\n")
    }
  }
}

pub fn reducer(
  acc: List(String),
  target_i: Int,
  current_i: Int,
  i_map: List(#(Int, String)),
  s_map: List(#(String, Int)),
) {
  let letter = i_map |> list.key_find(target_i - current_i) |> result.unwrap("")

  case letter {
    "A" -> {
      let line =
        string.repeat(" ", current_i) <> "A" <> string.repeat(" ", current_i)

      acc |> list.prepend(line)
    }
    _ -> {
      let space = s_map |> list.key_find(letter) |> result.unwrap(0)

      let line =
        string.repeat(" ", current_i)
        <> letter
        <> string.repeat(" ", space)
        <> letter
        <> string.repeat(" ", current_i)

      reducer(acc |> list.prepend(line), target_i, current_i + 1, i_map, s_map)
    }
  }
}
// pub fn main() {
//   echo build("A")
// }
