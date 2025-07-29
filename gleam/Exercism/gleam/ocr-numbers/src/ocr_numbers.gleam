import gleam/list
import gleam/regexp
import gleam/string

pub type Output {
  Unknown
  Digit(Int)
  List(List(Output))
}

pub type Error {
  InvalidLineNumber
  InvalidRowNumber
}

pub fn convert(input: String) -> Result(Output, Error) {
  let splits = input |> string.split("\n")

  let max_cols =
    splits
    |> list.map(fn(r) { { r |> string.length } % 3 == 0 })
    |> list.contains(False)
  let max_rows = { { splits |> list.length } - 1 } % 4 != 0

  case max_cols, max_rows {
    True, _ -> Error(InvalidLineNumber)
    False, True -> Error(InvalidRowNumber)
    False, False -> {
      let assert Ok(patt) = regexp.from_string("(.{3})")

      let lines =
        splits
        |> list.drop(1)
        |> list.index_map(fn(l, i) { #(i + 1, l) })
        |> list.fold([], fn(acc, cur) {
          let #(i, n) = cur

          let assert Ok(patt) = regexp.from_string("(.{3})")

          let chars =
            regexp.split(patt, n)
            |> list.filter(fn(s) { s != "" })
            |> list.index_map(fn(n, i) { #(i, [n]) })

          chars
          |> list.fold(acc, fn(acc, cur) {
            let #(y, m) = cur

            case acc |> list.key_find(y) {
              Ok(a) -> {
                acc |> list.key_set(y, a |> list.append(m))
              }
              _ -> {
                acc |> list.append([cur])
              }
            }
          })
        })

      echo lines

      Ok(Digit(1))
    }
  }
}

pub fn main() {
  echo convert(
    "
    _  _ 
  | _| _|
  ||_  _|
         
    _  _ 
|_||_ |_ 
  | _||_|
         
 _  _  _ 
  ||_||_|
  ||_| _|
         ",
  )
}
