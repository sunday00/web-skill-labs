import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string

type Puzzle {
  Puzzle(puzzle: String, left: List(String), right: String, all_i: Set(Int))
}

type Char {
  Char(txt: String, cur: Int, used: Set(Int))
}

pub fn solve(puzzle: String) -> Result(Dict(String, Int), Nil) {
  let assert [left, right] = puzzle |> string.split(" == ")
  let puzzle =
    Puzzle(
      puzzle,
      left |> string.split(" + "),
      right,
      list.range(0, 9) |> set.from_list,
    )

  echo resolve(puzzle)

  Ok(dict.new())
}

fn resolve(puzzle: Puzzle) {
  let all_letters =
    puzzle.left
    |> list.map(fn(l) { l |> string.to_graphemes })
    |> list.flatten
    |> list.append(puzzle.right |> string.to_graphemes)
    |> list.unique
    |> list.sort(string.compare)

  let state = do_with_nth_tails(puzzle, #([], ""), 1, 0)

  echo state
}

fn do_with_nth_tails(
  puzzle: Puzzle,
  nth_tail_str: #(List(String), String),
  col: Int,
  carry: Int,
) {
  let #(left_tails, right_tails) = nth_tail_str

  case left_tails |> list.length == 0 && right_tails == "" {
    True -> {
      do_with_nth_tails(puzzle, extract_nth_tails(puzzle, col), col, carry)
    }
    False -> {
      let splits =
        left_tails
        |> list.append([right_tails])
        |> list.map(fn(s) { s |> string.to_graphemes })
        |> list.flatten

      let map = get_next_mapping(dict.new(), splits, puzzle, splits, dict.new())
      let tmp_left_joins = left_tails |> string.join("|")
      let replaced_left_ints =
        map
        |> dict.fold(tmp_left_joins, fn(acc, ch, i) {
          acc |> string.replace(ch, i |> int.to_string)
        })
        |> string.split("|")
        |> list.map(fn(l) { l |> int.parse |> result.unwrap(0) })

      let replaced_right_ints =
        map
        |> dict.fold(right_tails, fn(acc, ch, i) {
          acc |> string.replace(ch, i |> int.to_string)
        })
        |> int.parse
        |> result.unwrap(0)

      case
        replaced_left_ints |> list.fold(0, fn(acc, cur) { acc + cur })
        == replaced_right_ints
      {
        True -> {
          todo
          // do do_with_nth_tails(.... col + 1)
        }
        False -> {
          todo
          // do get_next_mapping(...)
        }
      }
    }
  }
}

fn extract_nth_tails(puzzle: Puzzle, col: Int) {
  let left_tails =
    puzzle.left |> list.map(fn(s) { s |> string.slice(col * -1, col) })
  let right_tails = puzzle.right |> string.slice(col * -1, col)

  #(left_tails, right_tails)
}

fn get_next_mapping(
  acc: Dict(String, Int),
  txt: List(String),
  puzzle: Puzzle,
  fixed_text: List(String),
  used_map: Dict(String, List(Int)),
) {
  case txt {
    [f, ..r] -> {
      let prev_used = used_map |> dict.get(f) |> result.unwrap([])
      let current_other_letter_used = acc |> dict.values
      let canbe =
        puzzle.all_i
        |> set.difference(prev_used |> set.from_list)
        |> set.difference(current_other_letter_used |> set.from_list)
        |> set.to_list
        |> list.first

      case canbe {
        Ok(i) -> {
          // todo : no zero check

          get_next_mapping(
            acc |> dict.upsert(f, fn(_) { i }),
            r,
            puzzle,
            fixed_text,
            used_map,
          )
        }
        _ -> {
          todo
        }
      }
    }
    [] -> acc
  }
}

pub fn main() {
  echo solve("SEND + MORE == MONEY")
}
