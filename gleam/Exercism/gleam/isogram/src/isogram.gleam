import gleam/list
import gleam/string

pub fn is_isogram(phrase phrase: String) -> Bool {
  let l = phrase |> string.lowercase |> string.to_graphemes()

  l
  |> list.fold(True, fn(res, char) {
    case char {
      " " -> True
      "-" -> True
      _ -> {
        res
        && {
          phrase
          |> string.lowercase
          |> string.replace(char, "")
          |> string.length
          == { phrase |> string.length } - 1
        }
      }
    }
  })
}
