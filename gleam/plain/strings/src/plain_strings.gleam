import gleam/list
import gleam/string

pub fn main() {
  let k = "alphaca is good animal to feed"

  echo k
    // this works as split ""
    |> string.to_graphemes
    // this works as grouping count X, moving 1 letter
    |> list.window(3)
    |> list.map(string.concat)
}
