import gleam/list
import gleam/result
import gleam/string

const abc = "abcdefghijklmnopqrstuvwxyz"

pub fn encode(plaintext plaintext: String, key key: String) -> String {
  let i_abc = abc |> string.to_graphemes |> list.index_map(fn(n, i) { #(i, n) })
  let abc_i = abc |> string.to_graphemes |> list.index_map(fn(n, i) { #(n, i) })
  let i_key = key |> string.to_graphemes |> list.index_map(fn(n, i) { #(i, n) })

  plaintext
  |> string.to_graphemes
  |> list.index_map(fn(el, i) {
    let assert Ok(r) = abc_i |> list.key_find(el)
    let assert Ok(key) = i_key |> list.key_find(i % list.length(i_key))
    let assert Ok(plus) = abc_i |> list.key_find(key)

    { r + plus } % 26
  })
  |> list.map(fn(el) { i_abc |> list.key_find(el) |> result.unwrap("") })
  |> string.join("")
}

pub fn decode(ciphertext ciphertext: String, key key: String) -> String {
  let i_abc = abc |> string.to_graphemes |> list.index_map(fn(n, i) { #(i, n) })
  let abc_i = abc |> string.to_graphemes |> list.index_map(fn(n, i) { #(n, i) })
  let i_key = key |> string.to_graphemes |> list.index_map(fn(n, i) { #(i, n) })

  ciphertext
  |> string.to_graphemes
  |> list.index_map(fn(el, i) {
    let assert Ok(r) = abc_i |> list.key_find(el)
    let assert Ok(key) = i_key |> list.key_find(i % list.length(i_key))
    let assert Ok(minus) = abc_i |> list.key_find(key)

    let tmp_r = r - minus

    case tmp_r >= 0 {
      True -> tmp_r
      False -> {
        string.length(abc) + tmp_r
      }
    }
  })
  |> list.map(fn(el) { i_abc |> list.key_find(el) |> result.unwrap("") })
  |> string.join("")
}

pub fn generate_key() -> String {
  abc
}
