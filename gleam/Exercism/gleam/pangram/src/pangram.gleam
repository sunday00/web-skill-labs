import gleam/string

fn reducer(txt: String, cur_code: Int) {
  let assert Ok(abc) = string.utf_codepoint(cur_code)
  let abc = string.from_utf_codepoints([abc])

  case abc |> string.contains(txt, _) {
    False -> False
    _ -> {
      case cur_code {
        n if n == 122 -> True
        _ -> reducer(txt, cur_code + 1)
      }
    }
  }
}

pub fn is_pangram(sentence: String) -> Bool {
  reducer(sentence |> string.lowercase, 97)
}
