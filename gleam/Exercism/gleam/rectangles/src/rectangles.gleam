import gleam/dict.{type Dict}
import gleam/int
import gleam/list
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
          let map =
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

          let all_map =
            lines
            |> list.drop(1)
            |> list.index_fold(dict.new(), fn(acc, l, i) {
              l
              |> string.to_graphemes
              |> list.index_fold(acc, fn(ac, c, ii) {
                ac |> dict.insert(#(i, ii), c)
              })
            })

          let max_c =
            lines
            |> list.fold(0, fn(acc, cur) { int.max(acc, cur |> string.length) })

          reducer(0, map, all_map, { lines |> list.length }, max_c)
        }
      }
    }
  }
}

fn reducer(
  acc: Int,
  map: List(#(Int, Int)),
  all_map: Dict(#(Int, Int), String),
  max_l: Int,
  max_c: Int,
) {
  case map {
    [] -> acc
    [f, ..r] -> {
      let ac = get_cur_cnt(0, f, 1, 1, map, all_map, max_l, max_c)

      reducer(acc + ac, r, all_map, max_l, max_c)
    }
  }
}

fn get_cur_cnt(
  acc: Int,
  cur: #(Int, Int),
  nc: Int,
  nl: Int,
  map: List(#(Int, Int)),
  all_map: Dict(#(Int, Int), String),
  max_l: Int,
  max_c: Int,
) {
  case cur.0 + nl >= max_l {
    True -> acc
    False -> {
      let new_acc = case
        map |> list.contains(#(cur.0, cur.1 + nc)),
        map |> list.contains(#(cur.0 + nl, cur.1)),
        map |> list.contains(#(cur.0 + nl, cur.1 + nc))
      {
        True, True, True -> {
          case
            is_really_rectangle(
              cur,
              #(cur.0, cur.1 + nc),
              #(cur.0 + nl, cur.1),
              #(cur.0 + nl, cur.1 + nc),
              all_map,
            )
          {
            True -> acc + 1
            False -> acc
          }
        }
        _, _, _ -> {
          acc
        }
      }

      case cur.1 + nc >= max_c {
        True -> get_cur_cnt(new_acc, cur, 1, nl + 1, map, all_map, max_l, max_c)
        False ->
          get_cur_cnt(new_acc, cur, nc + 1, nl, map, all_map, max_l, max_c)
      }
    }
  }
}

fn is_really_rectangle(
  tl: #(Int, Int),
  tr: #(Int, Int),
  bl: #(Int, Int),
  br: #(Int, Int),
  all_map: Dict(#(Int, Int), String),
) {
  let is_valid_horizontal = case tr.1 - tl.1 == 1 {
    True -> True
    False ->
      check_ch_valid(tl.1 + 1, tr.1, tl.0, "-", all_map)
      && check_ch_valid(bl.1 + 1, br.1, bl.0, "-", all_map)
  }

  let is_valid_verticel = case bl.0 - tl.0 == 1 {
    True -> True
    False ->
      check_ch_valid(tl.0 + 1, bl.0, tl.1, "|", all_map)
      && check_ch_valid(tr.0 + 1, br.0, br.1, "|", all_map)
  }

  // echo #(tl, tr, bl, br, is_valid_horizontal && is_valid_verticel)

  is_valid_horizontal && is_valid_verticel
}

fn check_ch_valid(
  start: Int,
  end: Int,
  op: Int,
  shd: String,
  all_map: Dict(#(Int, Int), String),
) {
  case start == end {
    True -> True
    False -> {
      let point = case shd == "-" {
        True -> #(op, start)
        False -> #(start, op)
      }

      let real = dict.get(all_map, point)

      // echo #(start, end, shd, real)

      case real {
        Ok(s) -> {
          case s == shd || s == "+" {
            False -> False
            True -> check_ch_valid(start + 1, end, op, shd, all_map)
          }
        }
        _ -> False
      }
    }
  }
}
// pub fn main() {
//   echo rectangles(
//     "
//       +---+--+----+
//       |   +--+----+
//       +---+--+    |
//       |   +--+----+
//       +---+--+--+-+
//       +---+--+--+-+
//       +------+  | |
//                 +-+
//     ",
//   )
// }
