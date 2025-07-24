import gleam/list
import gleam/regexp
import gleam/result
import gleam/string

pub fn encode(phrase: String) -> String {
  let abc = abc() |> string.to_graphemes |> list.index_map(fn(n, i) { #(n, i) })
  let cba = cba() |> string.to_graphemes |> list.index_map(fn(n, i) { #(i, n) })

  let assert Ok(patt) = regexp.from_string("[^a-zA-Z0-9]")

  patt
  |> regexp.replace(phrase, "")
  |> string.lowercase
  |> string.to_graphemes
  |> list.map(fn(s) {
    let assert Ok(i) = abc |> list.key_find(s)
    cba |> list.key_find(i) |> result.unwrap("")
  })
  |> list.index_map(fn(s, i) {
    case { i + 1 } % 5 == 0 {
      True -> s <> " "
      False -> s
    }
  })
  |> string.concat
  |> string.trim_end
}

pub fn decode(phrase: String) -> String {
  let abc = abc() |> string.to_graphemes |> list.index_map(fn(n, i) { #(i, n) })
  let cba = cba() |> string.to_graphemes |> list.index_map(fn(n, i) { #(n, i) })

  phrase
  |> string.lowercase
  |> string.replace(" ", "")
  |> string.to_graphemes
  |> list.map(fn(s) {
    let assert Ok(i) = cba |> list.key_find(s)
    abc |> list.key_find(i) |> result.unwrap("")
  })
  |> string.concat
  |> string.trim_end
}

fn abc() {
  "abcdefghijklmnopqrstuvwxyz1234567890"
}

fn cba() {
  "zyxwvutsrqponmlkjihgfedcba1234567890"
}
// pub fn main() {
//   echo decode("zmlyh gzxov rhlug vmzhg vkkrm thglm v")
// }
