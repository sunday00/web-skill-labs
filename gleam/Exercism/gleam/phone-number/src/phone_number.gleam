import gleam/list
import gleam/regexp
import gleam/result
import gleam/string

pub fn clean(input: String) -> Result(String, String) {
  let assert Ok(patt) = regexp.from_string("[+ ()-.]")
  let res = regexp.replace(patt, input, "")

  case res |> string.length, res |> string.starts_with("1") {
    len, _ if len < 10 -> Error("must not be fewer than 10 digits")
    len, _ if len > 11 -> Error("must not be greater than 11 digits")
    len, False if len == 11 -> Error("11 digits must start with 1")
    len, _ -> {
      let assert Ok(patt1) = regexp.from_string("[a-zA-Z]")
      let assert Ok(patt2) = regexp.from_string("[^\\da-zA-Z]")

      case regexp.check(patt1, res), regexp.check(patt2, res) {
        True, _ -> Error("letters not permitted")
        _, True -> Error("punctuations not permitted")
        _, _ -> {
          let ins = case len {
            10 -> res
            _ -> res |> string.drop_start(1)
          }

          let assert Ok(sps) = regexp.from_string("(.{3})(.{3})(.{4})")
          let assert [ar, ex, ..] =
            regexp.split(sps, ins) |> list.filter(fn(s) { s != "" })

          case
            ar |> string.first |> result.unwrap(""),
            ex |> string.first |> result.unwrap("")
          {
            "0", _ -> Error("area code cannot start with zero")
            "1", _ -> Error("area code cannot start with one")
            _, "0" -> Error("exchange code cannot start with zero")
            _, "1" -> Error("exchange code cannot start with one")
            _, _ -> Ok(ins)
          }
        }
      }
    }
  }
}
