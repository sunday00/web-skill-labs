import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn rectangles(input: String) -> Int {
  case
    input
    |> string.to_graphemes
    |> list.filter(fn(el) { el == "+" })
    |> list.length
    < 4
  {
    True -> 0
    False -> {
      let lines = input |> string.split("\n")

      case lines |> list.length < 2 {
        True -> 0
        False -> {
          let assert map =
            lines
            |> list.drop(1)
            |> list.index_fold([], fn(acc, l, i) {
              l
              |> string.to_graphemes
              |> list.index_fold(acc, fn(ac, c, ii) {
                case c == "+" {
                  True -> ac |> list.append([#(i, ii)])
                  False -> ac
                }
              })
            })

          let max_c =
            lines
            |> list.fold(0, fn(acc, cur) { int.max(acc, cur |> string.length) })

          reducer(0, map, { lines |> list.length }, max_c)
        }
      }
    }
  }
}

fn reducer(acc: Int, map: List(#(Int, Int)), max_l: Int, max_c: Int) {
  case map {
    [] -> {
      todo
    }
    [f, ..r] -> {
      echo get_cur_cnt(0, f, 1, 1, map, max_l, max_c)
      // do next el
    }
  }

  2
}

fn get_cur_cnt(
  acc: Int,
  cur: #(Int, Int),
  nc: Int,
  nl: Int,
  map: List(#(Int, Int)),
  max_l: Int,
  max_c: Int,
) {
  echo #(map, #(cur.0, cur.1 + nc), #(cur.0 + nl, cur.1), #(
    cur.0 + nl,
    cur.1 + nc,
  ))
  case cur.0 + nl >= max_l || cur.1 + nc >= max_c {
    True -> acc
    False -> {
      let new_acc = case
        map |> list.contains(#(cur.0, cur.1 + nc)),
        map |> list.contains(#(cur.0 + nl, cur.1)),
        map |> list.contains(#(cur.0 + nl, cur.1 + nc))
      {
        True, True, True -> {
          acc + 1
        }
        _, _, _ -> {
          acc
        }
      }

      let ac = get_cur_cnt(new_acc, cur, nc + 1, nl, map, max_l, max_c)
      let ac = get_cur_cnt(ac, cur, nc, nl + 1, map, max_l, max_c)
      let ac = get_cur_cnt(ac, cur, nc + 1, nl + 1, map, max_l, max_c)

      new_acc + ac
    }
  }
}

pub fn main() {
  echo rectangles(
    "
      +-+
      | |
    +-+-+
    | | |
    +-+-+
    ",
  )
}
