import gleam/list
import gleam/string

pub fn score(word: String) -> Int {
  word
  |> string.to_graphemes
  |> list.fold(0, fn(acc, cur) { acc + conv_point(cur) })
}

fn conv_point(letter: String) {
  case letter |> string.lowercase {
    l if l == "d" || l == "g" -> 2
    l if l == "b" || l == "c" || l == "m" || l == "p" -> 3
    l if l == "f" || l == "h" || l == "v" || l == "w" || l == "y" -> 4
    l if l == "k" -> 5
    l if l == "j" || l == "x" -> 8
    l if l == "q" || l == "z" -> 10
    _ -> 1
  }
}
