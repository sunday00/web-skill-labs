import gleam/dict.{type Dict}
import gleam/list
import gleam/string

pub fn transform(legacy: Dict(Int, List(String))) -> Dict(String, Int) {
  legacy
  |> dict.to_list
  |> list.fold([], fn(acc, d) {
    let #(i, l_ab) = d

    let sub = l_ab |> list.map(fn(ab) { #(ab |> string.lowercase, i) })

    [acc, sub] |> list.concat
  })
  |> dict.from_list
}
