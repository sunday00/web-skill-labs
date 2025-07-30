import gleam/list
import gleam/regexp
import gleam/result
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
    True, _ -> Error(InvalidRowNumber)
    False, True -> Error(InvalidLineNumber)
    False, False -> {
      let assert Ok(patt) = regexp.from_string("(.{3})")

      let lists =
        splits
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

      let res =
        lists
        |> list.map(fn(l) {
          l
          |> list.map(fn(ll) {
            regexp.split(patt, ll)
            |> list.filter(fn(w) { w != "" })
            |> list.index_map(fn(x, i) { #(i, x) })
          })
          |> list.fold([], fn(acc, cur) {
            case acc |> list.length < cur |> list.length {
              True -> {
                cur
                |> list.map(fn(c) {
                  let #(i, n) = c
                  #(i, [n])
                })
              }
              False -> {
                cur
                |> list.map(fn(c) {
                  let #(i, n) = c
                  let ac = acc |> list.key_find(i) |> result.unwrap([])

                  #(i, ac |> list.append([n]))
                })
              }
            }
          })
          |> list.map(fn(l) {
            let #(_, s) = l
            plain_to_digit(s |> string.join(""))
          })
        })

      case res {
        [[value]] -> Ok(value)
        [[_, _, ..] as l] -> Ok(List(l))
        _ -> Ok(List(list.map(res, List)))
      }
    }
  }
}

fn plain_to_digit(s: String) {
  case s {
    " _ | ||_|" -> Digit(0)
    "     |  |" -> Digit(1)
    " _  _||_ " -> Digit(2)
    " _  _| _|" -> Digit(3)
    "   |_|  |" -> Digit(4)
    " _ |_  _|" -> Digit(5)
    " _ |_ |_|" -> Digit(6)
    " _   |  |" -> Digit(7)
    " _ |_||_|" -> Digit(8)
    " _ |_| _|" -> Digit(9)
    _ -> Unknown
  }
}
