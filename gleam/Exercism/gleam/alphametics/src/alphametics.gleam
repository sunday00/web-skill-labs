import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string

type Char {
  Char(cur: Int, used: Set(Int), zeroable: Bool)
}

type Manager {
  Manager(
    puzzle: String,
    no_zeros: Set(String),
    available: Set(Int),
    state: List(#(String, Char)),
  )
}

pub fn solve(puzzle: String) -> Result(Dict(String, Int), Nil) {
  let assert [left, right] = puzzle |> string.split(" == ")
  let no_zeros =
    left
    |> string.split(" + ")
    |> list.map(fn(l) { l |> string.first |> result.unwrap("") })
    |> list.prepend(right |> string.first |> result.unwrap(""))
    |> set.from_list

  let all_letters =
    puzzle
    |> string.replace(" == ", "")
    |> string.replace(" + ", "")
    |> string.to_graphemes
    |> list.unique
    |> list.sort(string.compare)

  let availables = list.range(0, 9) |> set.from_list

  let state = init([], all_letters, availables, no_zeros)

  let r =
    reducer(Manager(
      puzzle,
      no_zeros,
      availables
        |> set.difference(set.from_list(
          state
          |> list.map(fn(s) {
            let #(_, cha) = s
            cha.cur
          }),
        )),
      state |> list.reverse,
    ))

  // echo r

  Ok(dict.from_list([]))
}

fn init(
  acc: List(#(String, Char)),
  letters: List(String),
  available: Set(Int),
  no_zeros: Set(String),
) {
  case letters {
    [f, ..r] -> {
      let char = case set.contains(no_zeros, f) {
        True -> {
          let cur =
            available
            |> set.delete(0)
            |> set.to_list
            |> list.first
            |> result.unwrap(1)

          Char(cur, set.from_list([]), False)
        }
        False -> {
          let cur = available |> set.to_list |> list.first |> result.unwrap(1)

          Char(cur, set.from_list([]), True)
        }
      }

      init(
        acc |> list.prepend(#(f, char)),
        r,
        available |> set.delete(char.cur),
        no_zeros,
      )
    }
    [] -> acc
  }
}

fn reducer(manager: Manager) {
  let completed =
    manager.state
    |> list.fold(manager.puzzle, fn(acc, cur) {
      let #(txt, char) = cur
      acc |> string.replace(txt, char.cur |> int.to_string)
    })

  let assert [left, right] = completed |> string.split(" == ")
  case
    left
    |> string.split(" + ")
    |> list.fold(0, fn(acc, cur) {
      acc + { cur |> int.parse |> result.unwrap(0) }
    })
    == right |> int.parse |> result.unwrap(0)
  {
    True -> manager.state
    False -> {
      let assert Ok(#(txt, cha)) = manager.state |> list.last
      let assert Ok(#(_, lrs)) = manager.state |> list.key_pop(txt)

      let m = shift(Manager(..manager, state: lrs), [#(txt, cha)])

      reducer(m)
    }
  }
}

fn shift(manager: Manager, letters: List(#(String, Char))) {
  echo manager.state
  echo letters

  case letters {
    [f, ..r] -> {
      let #(txt, char) = f
      let available = manager.available
      let next_char = reset_char(char, available)

      case next_char.cur >= 0 {
        True -> {
          let new_state =
            init(
              [],
              r
                |> list.map(fn(el) {
                  let #(txt, _) = el
                  txt
                }),
              available |> set.delete(next_char.cur),
              manager.no_zeros,
            )
            |> list.prepend(#(txt, next_char))

          Manager(
            ..manager,
            state: manager.state |> list.append(new_state),
            available: manager.available |> set.delete(next_char.cur),
          )
        }
        False -> {
          let last_res = manager.state |> list.last

          case last_res {
            Ok(last) -> {
              let #(lt, lch) = last
              let assert Ok(#(_, lrs)) = manager.state |> list.key_pop(lt)
              let new_avail = manager.available |> set.union(char.used)

              shift(
                Manager(..manager, state: lrs, available: new_avail),
                init(
                  [],
                  letters
                    |> list.map(fn(rr) {
                      let #(t, _) = rr
                      t
                    }),
                  new_avail,
                  manager.no_zeros,
                )
                  |> list.prepend(last),
              )
            }
            _ -> {
              todo
            }
          }
        }
      }
    }
    [] -> manager
  }
}

fn reset_char(char: Char, available: Set(Int)) {
  let cur = case char.zeroable {
    True -> available |> set.delete(char.cur) |> set.to_list |> list.first
    False ->
      available
      |> set.delete(char.cur)
      |> set.delete(0)
      |> set.to_list
      |> list.first
  }

  case cur {
    Ok(c) -> Char(..char, cur: c, used: char.used |> set.insert(char.cur))
    _ -> Char(..char, cur: -1, used: set.new())
  }
}

pub fn main() {
  echo solve("SEND + MORE == MONEY")
}
