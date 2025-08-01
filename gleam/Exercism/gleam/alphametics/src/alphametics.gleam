import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/string

type Manager {
  Manager(
    current_map: List(#(String, Int)),
    words: List(String),
    answer: String,
    skip: Bool,
    error: Int,
  )
}

pub fn solve(puzzle: String) -> Result(Dict(String, Int), Nil) {
  let assert [state, answer] = puzzle |> string.split(" == ")

  let words =
    state
    |> string.split(" + ")

  let strings =
    words
    |> list.append([answer])
    |> list.flat_map(fn(l) { l |> string.split("") })
    |> list.unique
    |> list.sort(string.compare)

  let manager =
    Manager(
      strings |> list.index_map(fn(s, i) { #(s, i) }),
      words,
      answer,
      False,
      0,
    )

  let res = reducer(manager)

  case res.error == 1 {
    True -> Error(Nil)
    False -> Ok(dict.from_list(res.current_map))
  }
}

fn reducer(manager: Manager) {
  case
    manager.words
    |> list.map(fn(w) { map_to_int(w, manager) })
    |> list.reduce(fn(acc, cur) { acc + cur })
    |> result.unwrap(0)
    == map_to_int(manager.answer, manager)
    && !manager.skip
  {
    True -> {
      case
        [manager.answer, ..manager.words]
        |> list.fold(True, fn(acc, cur) {
          acc
          && manager.current_map
          |> list.key_find(cur |> string.first |> result.unwrap(""))
          |> result.unwrap(0)
          != 0
        })
      {
        True -> manager
        False -> {
          reducer(Manager(..manager, skip: True))
        }
      }
    }
    False -> {
      let current_map_int = {
        manager.current_map
        |> list.fold("", fn(acc, el) {
          let #(_k, v) = el
          acc <> { v |> int.to_string }
        })
      }

      let next_int =
        increase_int(current_map_int)
        |> string.to_graphemes

      case next_int |> list.length > manager.current_map |> list.length {
        True -> {
          Manager(..manager, error: 1)
        }
        False -> {
          let next_int = case
            next_int |> list.length < manager.current_map |> list.length
          {
            True -> {
              next_int
              |> list.prepend("0")
              |> list.index_map(fn(s, i) { #(i, s) })
            }
            False -> next_int |> list.index_map(fn(s, i) { #(i, s) })
          }

          reducer(
            Manager(
              ..manager,
              current_map: manager.current_map
                |> list.index_map(fn(el, i) {
                  let #(k, _) = el

                  let assert Ok(target) = next_int |> list.key_find(i)

                  #(k, target |> int.parse |> result.unwrap(0))
                }),
              skip: False,
            ),
          )
        }
      }
    }
  }
}

fn map_to_int(word: String, manager: Manager) {
  word
  |> string.to_graphemes
  |> list.map(fn(s) {
    let assert Ok(ss) = manager.current_map |> list.key_find(s)
    ss |> int.to_string
  })
  |> string.join("")
  |> int.parse
  |> result.unwrap(0)
}

fn increase_int(i: String) {
  let next =
    i
    |> int.parse
    |> result.unwrap(0)
    |> int.add(1)
    |> int.to_string()
    |> string.to_graphemes

  let unique = next |> list.unique

  case list.length(unique) == list.length(next) {
    True -> next |> string.join("")
    False -> increase_int(next |> string.join(""))
  }
}

pub fn main() {
  echo solve("AS + A == MOM")
}
