import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string

type Manager {
  Manager(
    puzzle: String,
    letters: List(String),
    zeroable: Dict(String, Bool),
    states: Dict(String, Int),
    availables: Set(Int),
  )
}

pub fn solve(puzzle: String) -> Result(Dict(String, Int), Nil) {
  let assert [left, right] = puzzle |> string.split(" == ")
  let no_zeros =
    left
    |> string.split(" + ")
    |> list.map(fn(l) { l |> string.first |> result.unwrap("") })
    |> list.prepend(right |> string.first |> result.unwrap(""))

  let all_letters =
    puzzle
    |> string.replace(" == ", "")
    |> string.replace(" + ", "")
    |> string.to_graphemes
    |> list.unique
    |> list.sort(string.compare)

  let manager =
    Manager(
      puzzle,
      all_letters,
      all_letters
        |> list.map(fn(el) {
          case no_zeros |> list.contains(el) {
            True -> #(el, False)
            False -> #(el, True)
          }
        })
        |> dict.from_list,
      dict.new(),
      list.range(0, 9) |> set.from_list,
    )

  echo reducer(manager)

  Ok(dict.from_list([]))
}

fn reducer(manager: Manager) {
  let used = manager.states |> dict.values |> set.from_list

  manager.availables
  |> set.difference(used)
  |> set.to_list
  |> list.fold([], fn(acc, i) {
    case manager.letters {
      [f, ..r] -> {
        let new_m =
          Manager(
            ..manager,
            letters: r,
            states: manager.states |> dict.insert(f, i),
          )

        process.sleep(1000)
        echo reducer(new_m)
      }
      [] -> {
        echo manager.states

        [manager]
      }
    }
  })
}

pub fn main() {
  echo solve("SEND + MORE == MONEY")
}
