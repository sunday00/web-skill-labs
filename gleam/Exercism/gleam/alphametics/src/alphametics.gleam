import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/string

type Char {
  Char(txt: String, cur: Int, used: List(Int), zeroable: Bool)
}

type Manager {
  Manager(chars: List(Char), words: List(String), answer: String)
}

const initial_candidates = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

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

  let non_zeros =
    words
    |> list.append([answer])
    |> list.map(fn(el) { el |> string.first |> result.unwrap("") })
    |> list.unique
    |> list.sort(string.compare)

  let chars =
    generator([], strings, non_zeros, initial_candidates) |> list.reverse

  case reducer(Manager(chars: chars, words: words, answer: answer)) {
    Ok(res) ->
      Ok(
        res |> list.map(fn(cha: Char) { #(cha.txt, cha.cur) }) |> dict.from_list,
      )
    _ -> Error(Nil)
  }
}

fn generator(
  acc: List(Char),
  strings: List(String),
  non_zeros: List(String),
  candidates: List(Int),
) {
  case strings {
    [f, ..r] -> {
      let #(char, rest_candidates) = case non_zeros |> list.contains(f) {
        True -> {
          let assert [min, ..] = candidates |> list.filter(fn(c) { c != 0 })
          #(
            Char(f, min, [0], False),
            candidates |> list.filter(fn(c) { c != min }),
          )
        }
        False -> {
          let assert [min, ..] = candidates
          #(
            Char(f, min, [], True),
            candidates |> list.filter(fn(c) { c != min }),
          )
        }
      }

      generator(list.prepend(acc, char), r, non_zeros, rest_candidates)
    }
    [] -> acc
  }
}

fn reducer(manager: Manager) {
  let sum =
    manager.words
    |> list.fold(0, fn(acc, cur) { acc + { cur |> word_to_int(manager) } })

  case sum == manager.answer |> word_to_int(manager) {
    True -> manager.chars |> Ok
    False -> {
      let usings = manager.chars |> list.map(fn(ch) { ch.cur })

      case
        shift(
          manager.chars |> list.reverse,
          initial_candidates
            |> list.filter(fn(c) { !{ usings |> list.contains(c) } }),
        )
      {
        Ok(chars) -> {
          // echo chars
          // process.sleep(1000)

          reducer(Manager(..manager, chars: chars |> list.reverse))
        }
        _ -> Error(Nil)
      }
    }
  }
}

fn word_to_int(word: String, manager: Manager) {
  word
  |> string.to_graphemes
  |> list.fold("", fn(acc, cur) {
    let assert Ok(char) = manager.chars |> list.find(fn(cha) { cha.txt == cur })
    acc <> char.cur |> int.to_string
  })
  |> int.parse
  |> result.unwrap(0)
}

fn shift(prev: List(Char), candidates: List(Int)) {
  case prev {
    [char, ..r] -> {
      let remains = case char.zeroable {
        True ->
          candidates
          |> list.filter(fn(c) {
            !{
              r |> list.map(fn(rc) { rc.cur }) |> list.contains(c)
              || char.used |> list.contains(c)
              || c == char.cur
            }
          })
        False ->
          candidates
          |> list.filter(fn(c) {
            !{
              r |> list.map(fn(rc) { rc.cur }) |> list.contains(c)
              || char.used |> list.contains(c)
              || c == char.cur
              || c == 0
            }
          })
      }

      case remains {
        [f, ..] -> {
          r
          |> list.prepend(
            Char(..char, cur: f, used: char.used |> list.prepend(char.cur)),
          )
          |> Ok
        }
        [] -> {
          case shift(r, list.range(0, 9)) {
            Ok(head) -> {
              let cur =
                candidates
                |> list.filter(fn(el) {
                  case char.zeroable {
                    True -> {
                      !{
                        head
                        |> list.map(fn(h: Char) { h.cur })
                        |> list.contains(el)
                      }
                    }
                    False -> {
                      !{
                        head
                        |> list.map(fn(h: Char) { h.cur })
                        |> list.contains(el)
                        || el == 0
                      }
                    }
                  }
                })
                |> list.first
                |> result.unwrap(0)

              head |> list.prepend(Char(..char, cur: cur, used: [])) |> Ok
            }
            Error(_) -> Error(Nil)
          }
        }
      }
    }
    [] -> {
      Error(Nil)
    }
  }
}
// pub fn main() {
//   echo solve("SEND + MORE == MONEY")
// }
