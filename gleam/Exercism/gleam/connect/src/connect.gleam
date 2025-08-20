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

  Ok(O)
}

fn check_o_winner(board: String) {
  let lines = board |> string.split("\n")

  let line_length = lines |> list.length

  let lines = case line_length {
    len if len == 0 -> {
      // err
      todo
    }
    len if len == 1 -> {
      // just x, o 
      todo
    }
    _ -> lines |> list.drop(1)
  }

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
          echo path_find_o(
            [],
            #(0, f),
            line_length,
            col_length,
            o_map,
            r |> list.map(fn(el) { #(0, el) }),
          )
        }
        [] -> {
          todo
        }
      }
    }
    _ -> {
      todo
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
      el.0 >= 0 && el.1 >= 0 && el.0 < line_length && el.1 < line_length
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
      echo acc
      echo cur
      echo choices
      todo
    }
  }
}

pub fn main() {
  echo winner(
    "
. O . .
 O X X X
  O O O .
   X X O X
    . O X .",
  )
}
