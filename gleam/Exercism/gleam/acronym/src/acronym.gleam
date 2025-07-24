import gleam/list
import gleam/regexp
import gleam/string

pub fn abbreviate(phrase phrase: String) -> String {
  let assert Ok(patt) = regexp.from_string("[^a-zA-Z\\s\\-]")
  regexp.replace(patt, phrase, "")
  |> string.split(" ")
  |> list.fold([], reducer)
  |> string.join("")
}

fn reducer(acc: List(String), cur: String) {
  case cur |> string.contains("-"), cur == "-" {
    True, False -> {
      let add =
        cur
        |> string.split("-")
        |> list.map(fn(s) {
          let assert Ok(ss) = s |> string.first
          ss |> string.uppercase
        })
      acc |> list.append(add)
    }
    True, True -> {
      acc
    }
    _, _ -> {
      let assert Ok(c) = cur |> string.first
      acc |> list.append([c |> string.uppercase])
    }
  }
}
// pub fn main() {
//   echo abbreviate("Something - I made up from thin air")
// }
