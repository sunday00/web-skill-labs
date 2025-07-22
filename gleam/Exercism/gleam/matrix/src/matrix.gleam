import gleam/int
import gleam/list
import gleam/string

pub fn row(index: Int, string: String) -> Result(List(Int), Nil) {
  let mid =
    string
    |> string.split("\n")
    |> list.index_map(fn(r, i) { #(i + 1, r) })
    |> list.key_find(index)

  case mid {
    Error(_) -> Error(Nil)
    Ok(row) -> {
      let l =
        row
        |> string.split(" ")
        |> list.map(fn(s) {
          let assert Ok(i) = s |> int.parse
          i
        })

      Ok(l)
    }
  }
}

pub fn column(index: Int, string: String) -> Result(List(Int), Nil) {
  let columns =
    string
    |> string.split("\n")
    |> list.map(fn(row) {
      let mid =
        row
        |> string.split(" ")
        |> list.index_map(fn(ch, i) { #(i + 1, ch) })
        |> list.key_find(index)

      case mid {
        Ok(m) -> m
        Error(_) -> "E"
      }
    })

  echo columns

  case columns |> list.contains("E") {
    True -> Error(Nil)
    False -> {
      let l =
        columns
        |> list.map(fn(s) {
          let assert Ok(i) = s |> int.parse

          i
        })

      Ok(l)
    }
  }
}
