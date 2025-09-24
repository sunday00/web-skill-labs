import gleam/float
import gleam/int
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string

pub fn ciphertext(plaintext: String) -> String {
  let assert Ok(patt) = regexp.from_string("[a-z0-9]")

  let no =
    plaintext
    |> string.lowercase
    |> string.to_graphemes
    |> list.filter(fn(el) { regexp.check(patt, el) })

  let l = no |> list.length

  let r =
    int.square_root(l)
    |> result.unwrap(0.0)
    |> float.ceiling
    |> float.round

  let c = case r * r, r * { r - 1 } {
    _, s if s <= l -> r
    _, _ -> r - 1
  }

  reducer(list.repeat("", r) |> list.index_map(fn(s, i) { #(i, s) }), 0, no, c)
}

fn reducer(acc: List(#(Int, String)), cur: Int, rem: List(String), max_col: Int) {
  case rem {
    [] -> {
      acc
      |> list.map(fn(l) {
        let #(_, s) = l

        echo #(s, max_col)

        s |> string.pad_end(max_col, " ")
      })
      |> string.join(" ")
    }
    [f, ..r] -> {
      let assert Ok(cur_line) = acc |> list.key_find(cur)
      reducer(
        acc |> list.key_set(cur, cur_line <> f),
        { cur + 1 } % { acc |> list.length },
        r,
        max_col,
      )
    }
  }
}
