import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string

pub type Player {
  X
  O
}

pub fn winner(board: String) -> Result(Player, Nil) {
  let res_o = check_o_winner(board)
  let res_x = check_x_winner(board)

  case res_o, res_x {
    True, False -> Ok(O)
    False, True -> Ok(X)
    _, _ -> Error(Nil)
  }
}

fn check_o_winner(board: String) {
  let lines = board |> string.split("\n")

  let line_length = lines |> list.length

  case line_length {
    len if len == 0 -> False
    len if len == 1 -> board == "O"
    _ -> {
      let lines = lines |> list.drop(1)
      let col_length =
        lines
        |> list.first
        |> result.unwrap("")
        |> string.replace(" ", "")
        |> string.length

      let o_map =
        lines
        |> list.index_fold(dict.new(), fn(acc, cur, i) {
          let l =
            cur
            |> string.replace(" ", "")
            |> string.to_graphemes
            |> list.index_map(fn(ch, ii) { #(ch, ii) })
            |> list.filter(fn(el) { el.0 == "O" })
            |> list.map(fn(el) { el.1 })

          acc |> dict.insert(i, l)
        })

      case o_map |> dict.get(0) {
        Ok(l) -> {
          case l {
            [f, ..r] -> {
              path_find_o(
                [],
                #(0, f),
                line_length - 1,
                col_length,
                o_map,
                r |> list.map(fn(el) { #(0, el) }),
              )
            }
            [] -> False
          }
        }
        _ -> False
      }
    }
  }
}

fn path_find_o(
  acc: List(#(Int, Int)),
  cur: #(Int, Int),
  line_length: Int,
  col_length: Int,
  map: Dict(Int, List(Int)),
  choices: List(#(Int, Int)),
) {
  let #(line, value) = cur

  case line + 1 == line_length {
    True -> True
    False -> {
      let neigbor =
        [
          #(line + 1, value),
          #(line + 1, value - 1),
          #(line, value + 1),
          #(line, value - 1),
          #(line - 1, value),
          #(line - 1, value + 1),
        ]
        |> list.filter(fn(el) {
          el.0 >= 0 && el.1 >= 0 && el.0 < line_length && el.1 < col_length
        })

      let possibles =
        neigbor
        |> list.filter(fn(el) {
          case map |> dict.get(el.0) {
            Ok(l) -> l |> list.contains(el.1) && !{ acc |> list.contains(el) }
            _ -> False
          }
        })

      case possibles {
        [f, ..r] -> {
          path_find_o(
            acc |> list.prepend(cur),
            f,
            line_length,
            col_length,
            map,
            choices |> list.append(r),
          )
        }
        [] -> {
          case choices {
            [f, ..r] ->
              path_find_o(
                acc |> list.prepend(cur),
                f,
                line_length,
                col_length,
                map,
                r,
              )
            [] -> False
          }
        }
      }
    }
  }
}

fn check_x_winner(board: String) {
  let lines = board |> string.split("\n")

  let line_length = lines |> list.length

  case line_length {
    len if len == 0 -> False
    len if len == 1 -> board == "X"
    _ -> {
      let lines = lines |> list.drop(1)
      let col_length =
        lines
        |> list.first
        |> result.unwrap("")
        |> string.replace(" ", "")
        |> string.length

      let x_map =
        lines
        |> list.index_fold(dict.new(), fn(acc, cur, i) {
          let l =
            cur
            |> string.replace(" ", "")
            |> string.to_graphemes
            |> list.index_map(fn(ch, ii) { #(ch, ii) })
            |> list.filter(fn(el) { el.0 == "X" })
            |> list.map(fn(el) { el.1 })

          l
          |> list.fold(acc, fn(ac, cu) {
            case ac |> dict.get(cu) {
              Ok(prev) -> ac |> dict.insert(cu, prev |> list.append([i]))
              _ -> ac |> dict.insert(cu, [i])
            }
          })
        })

      case x_map |> dict.get(0) {
        Ok(l) -> {
          case l {
            [f, ..r] -> {
              path_find_x(
                [],
                #(0, f),
                line_length - 1,
                col_length,
                x_map,
                r |> list.map(fn(el) { #(0, el) }),
              )
            }
            [] -> False
          }
        }
        _ -> False
      }
    }
  }
}

fn path_find_x(
  acc: List(#(Int, Int)),
  cur: #(Int, Int),
  line_length: Int,
  col_length: Int,
  map: Dict(Int, List(Int)),
  choices: List(#(Int, Int)),
) {
  let #(col, line) = cur

  case col == col_length - 1 {
    True -> True
    False -> {
      let neigbor =
        [
          #(col, line + 1),
          #(col - 1, line + 1),
          #(col + 1, line),
          #(col - 1, line),
          #(col, line - 1),
          #(col + 1, line - 1),
        ]
        |> list.filter(fn(el) {
          el.0 >= 0 && el.1 >= 0 && el.0 < col_length && el.1 < line_length
        })

      let possibles =
        neigbor
        |> list.filter(fn(el) {
          case map |> dict.get(el.0) {
            Ok(l) -> l |> list.contains(el.1) && !{ acc |> list.contains(el) }
            _ -> False
          }
        })

      case possibles {
        [f, ..r] -> {
          path_find_x(
            acc |> list.prepend(cur),
            f,
            line_length,
            col_length,
            map,
            choices |> list.append(r),
          )
        }
        [] -> {
          case choices {
            [f, ..r] ->
              path_find_x(
                acc |> list.prepend(cur),
                f,
                line_length,
                col_length,
                map,
                r,
              )
            [] -> False
          }
        }
      }
    }
  }
}
// pub fn main() {
//   echo winner(
//     "
// . . . . .
//  . . . . .
//   . . . . .
//    . . . . .
//     . . . . .",
//   )
// }
