import gleam/regex
import gleam/string

pub fn is_valid_line(line: String) -> Bool {
  let assert Ok(r) = regex.from_string("\\[(DEBUG|ERROR|INFO|WARNING)\\] .+")

  regex.check(r, line)
}

pub fn split_line(line: String) -> List(String) {
  let assert Ok(r) = regex.from_string("<[~*=-]*>")

  regex.split(r, line)
}

pub fn tag_with_user_name(line: String) -> String {
  let assert Ok(r) = regex.from_string("User\\s+.+?(\\s|$)")

  case regex.scan(r, line) {
    [mat] -> {
      "[USER] "
      <> { mat.content |> string.replace("User", "") |> string.trim }
      <> " "
      <> line
    }
    _ -> line
  }
}
