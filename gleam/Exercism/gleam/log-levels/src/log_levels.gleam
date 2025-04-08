import gleam/iterator
import gleam/list
import gleam/option
import gleam/pair
import gleam/regex
import gleam/result
import gleam/string

// string can concat with <>

pub fn message(log_line: String) -> String {
  string.crop(log_line, "]:")
  |> string.replace("]:", "")
  |> string.trim
}

pub fn log_level(log_line: String) -> String {
  log_line
  |> string.crop("[")
  |> string.replace("[", "")
  |> string.split_once("]: ")
  |> result.unwrap(#("", ""))
  |> pair.first
  |> string.lowercase
}

pub fn reformat(log_line: String) -> String {
  // let assert Ok(reg) = regex.from_string("\\[(\\w+)\\]:")

  // let assert Ok(op) = regex.scan(reg, log_line) |> list.last

  // let lv = op.submatches |> list.first |> result.unwrap(option.Some("")) |> option.unwrap("") |> string.lowercase

  message(log_line) <> " (" <> log_level(log_line) <> ")"
}
// fn tes(some: String) -> String {
//   case some {
//     "pref" <> rest -> rest
//     _ -> "failed"
//   }
// }

// pub fn main() {
//   echo reformat("[ERROR]: \t Corrupt disk\t \t \r\n")
// }
