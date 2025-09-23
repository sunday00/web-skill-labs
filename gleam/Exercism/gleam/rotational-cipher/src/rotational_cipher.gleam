import gleam/list
import gleam/result
import gleam/string

const uabc = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

const labc = "abcdefghijklmnopqrstuvwxyz"

pub fn rotate(shift_key: Int, text: String) -> String {
  let i_uabc =
    uabc |> string.to_graphemes |> list.index_map(fn(n, i) { #(i, n) })
  let uabc_i =
    uabc |> string.to_graphemes |> list.index_map(fn(n, i) { #(n, i) })

  let i_labc =
    labc |> string.to_graphemes |> list.index_map(fn(n, i) { #(i, n) })
  let labc_i =
    labc |> string.to_graphemes |> list.index_map(fn(n, i) { #(n, i) })

  let list =
    text
    |> string.to_graphemes
    |> list.map(fn(el) {
      case uabc_i |> list.key_find(el), labc_i |> list.key_find(el) {
        Ok(ii), Error(_) -> {
          let n = { ii + shift_key } % 26
          i_uabc |> list.key_find(n) |> result.unwrap("")
        }
        Error(_), Ok(ii) -> {
          let n = { ii + shift_key } % 26

          i_labc |> list.key_find(n) |> result.unwrap("")
        }
        _, _ -> el
      }
    })

  list |> string.join("")
}
