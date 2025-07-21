import gleam/list
import gleam/regexp
import gleam/result
import gleam/string

pub fn is_paired(value: String) -> Bool {
  let assert Ok(patt) = regexp.from_string("\\[|\\]|\\{|\\}|\\(|\\)")

  let summaries = regexp.match_map(patt, value, fn(m) { m.content })

  let res = [True]

  let _ =
    summaries
    |> string.to_graphemes()

  // TODO
  // insert list opposite }, ], ) 
  // loop values then meet }, ], ) , compare pop char from above list
  // if flasy -> result is false.

  res |> list.reduce(fn(acc, b) { acc && b }) |> result.unwrap(True)
}

pub fn main() {
  echo is_paired("{)()")
}
