import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string

type Puzzle {
  Puzzle(puzzle: String, left: List(String), right: String)
}

pub fn solve(puzzle: String) -> Result(Dict(String, Int), Nil) {
  let assert [left, right] = puzzle |> string.split(" == ")
  let puzzle = Puzzle(puzzle, left |> string.split(" + "), right)

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

  let init_map =
    all_letters
    |> list.map(fn(ch) { #(ch, -1) })
    |> dict.from_list

  let init_used =
    all_letters |> list.map(fn(ch) { #(ch, []) }) |> dict.from_list

  let all_i = list.range(0, 9) |> set.from_list

  track(puzzle, init_map, init_used, all_i, 1, 1)
}

fn track(
  puzzle: Puzzle,
  map: Dict(String, Int),
  used: Dict(String, List(Int)),
  all_i: Set(Int),
  col: Int,
  carry: Int,
) {
  let lefts =
    puzzle.left
    |> list.map(fn(el) {
      el |> string.slice(-1 * col, col) |> string.pad_start(col, "0")
    })

  let rights =
    puzzle.right |> string.slice(-1 * col, col) |> string.pad_start(col, "0")

  let targets =
    lefts
    |> list.map(fn(l) { l |> string.to_graphemes })
    |> list.flatten
    |> list.append(rights |> string.to_graphemes)
    |> list.unique
    |> list.sort(string.compare)

  let state =
    all_i
    |> set.map(fn(i) { #() })

  echo state

  todo
}

pub fn main() {
  echo solve("SEND + MORE == MONEY")
}
