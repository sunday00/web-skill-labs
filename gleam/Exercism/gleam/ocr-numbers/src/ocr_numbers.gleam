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

      echo splits
        |> list.drop(0)
        |> list.index_map(fn(line, idx) { #(idx, line) })
        |> list.fold([], fn(acc, cur) {
          let #(i, l) = cur

          case i % 4 {
            0 -> {
              acc
              |> list.map(fn(ac) {
                let #(_k, v) = ac

                #("", v)
              })
            }
            1 -> {
              acc |> list.append([#("ing", [l])])
            }
            _ -> {
              let assert Ok(target) = acc |> list.key_find("ing")
              acc |> list.key_set("ing", target |> list.append([l]))
            }
          }
        })
        |> list.map(fn(l) {
          let #(_, v) = l
          v
        })

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
