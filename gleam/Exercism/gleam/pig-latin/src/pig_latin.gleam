import gleam/list
import gleam/string

pub fn translate(phrase: String) -> String {
  phrase |> string.split(" ") |> list.map(word_modifier) |> string.join(" ")
}

fn word_modifier(phrase: String) {
  let rule1 =
    phrase |> string.starts_with("a")
    || phrase |> string.starts_with("e")
    || phrase |> string.starts_with("i")
    || phrase |> string.starts_with("o")
    || phrase |> string.starts_with("u")
    || phrase |> string.starts_with("xr")
    || phrase |> string.starts_with("yt")

  let rule3 = phrase |> string.contains("qu")

  let rule4 = is_rule_4(phrase)

  case rule1, rule3, rule4 {
    True, _, _ -> {
      phrase <> "ay"
    }
    False, True, _ -> {
      let s = phrase |> string.crop("qu")
      let f = phrase |> string.replace(s, "")

      { s |> string.replace("qu", "") } <> f <> "qu" <> "ay"
    }
    False, False, True -> {
      let s = phrase |> string.crop("y")
      let f = phrase |> string.replace(s, "")

      s <> f <> "ay"
    }
    _, _, _ -> {
      let i = find_index_first_vowel(0, phrase)

      case i {
        -1 -> {
          phrase <> "ay"
        }
        _ -> {
          let s = phrase |> string.drop_start(i)
          let f = phrase |> string.replace(s, "")

          s <> f <> "ay"
        }
      }
    }
  }
}

fn find_index_first_vowel(i: Int, word: String) {
  case string.first(word) {
    Ok(f) -> {
      case ["a", "e", "i", "o", "u"] |> list.contains(f) {
        True -> {
          i
        }
        False -> find_index_first_vowel(i + 1, string.drop_start(word, 1))
      }
    }
    Error(_) -> {
      -1
    }
  }
}

fn find_index_first_y(i: Int, word: String) {
  case string.first(word) {
    Ok(f) -> {
      case f == "y" {
        True -> {
          i
        }
        False -> find_index_first_vowel(i + 1, string.drop_start(word, 1))
      }
    }
    Error(_) -> {
      -1
    }
  }
}

fn is_rule_4(word: String) {
  let first_v = find_index_first_vowel(0, word)

  word |> string.contains("y")
  && !{ word |> string.starts_with("y") }
  && { first_v < 0 || first_v > find_index_first_y(0, word) }
}

pub fn main() {
  echo translate("my")
}
