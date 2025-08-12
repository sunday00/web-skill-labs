import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string

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

  let tails =
    left
    |> string.split(" + ")
    |> list.map(fn(el) { el |> string.last |> result.unwrap("") })

  let parsed = resolve([], all_letters, no_zeros, tails, puzzle, right)

  Ok(dict.from_list([]))
}

fn resolve(
  acc: List(Dict(String, Int)),
  all_letters: List(String),
  no_zeros: List(String),
  lasts_letters: List(String),
  puzzle: String,
  right: String,
) {
  let answer_last = right |> string.last |> result.unwrap("")

  let last_combinations =
    lasts(
      [],
      no_zeros,
      lasts_letters |> list.append([answer_last]),
      lasts_letters,
      answer_last,
    )

  let feel_texts =
    all_letters |> set.from_list |> set.difference(set.from_list(lasts_letters))

  echo completer(last_combinations, feel_texts |> set.to_list, no_zeros, puzzle)
}

fn lasts(
  acc: List(Dict(String, Int)),
  no_zeros: List(String),
  texts: List(String),
  fixed_lasts: List(String),
  answer_last: String,
) -> List(Dict(String, Int)) {
  case acc |> list.length {
    0 -> {
      let assert Ok(f) = texts |> list.first

      let rng = case list.contains(no_zeros, f) {
        True -> list.range(1, 9)
        False -> list.range(0, 9)
      }

      lasts(
        rng |> list.map(fn(i) { dict.from_list([#(f, i)]) }),
        no_zeros,
        texts |> list.drop(1),
        fixed_lasts,
        answer_last,
      )
    }
    _ -> {
      let f = texts |> list.first

      case f {
        Ok(ft) -> {
          let new_acc =
            acc
            |> list.map(fn(ac) {
              case ac |> dict.get(ft) {
                Ok(_) -> [ac]
                _ -> {
                  let used = ac |> dict.values() |> set.from_list
                  let rem =
                    case list.contains(no_zeros, ft) {
                      True -> list.range(1, 9)
                      False -> list.range(0, 9)
                    }
                    |> set.from_list
                    |> set.difference(used)

                  rem
                  |> set.map(fn(i) { ac |> dict.insert(ft, i) })
                  |> set.to_list
                }
              }
            })
            |> list.flatten

          lasts(
            new_acc,
            no_zeros,
            texts |> list.drop(1),
            fixed_lasts,
            answer_last,
          )
        }
        _ -> {
          acc
          |> list.filter(fn(l) {
            let sum =
              fixed_lasts
              |> list.fold(0, fn(acc, cur) {
                acc + { l |> dict.get(cur) |> result.unwrap(0) }
              })
            let answer_int = l |> dict.get(answer_last) |> result.unwrap(0)

            sum % 10 == answer_int
          })
        }
      }
    }
  }
}

fn completer(
  acc: List(Dict(String, Int)),
  texts: List(String),
  no_zeros: List(String),
  puzzle: String,
) -> List(Dict(String, Int)) {
  let f = texts |> list.first

  case f {
    Ok(ft) -> {
      let new_acc =
        acc
        |> list.map(fn(ac) {
          let used = ac |> dict.values() |> set.from_list
          let rem =
            case list.contains(no_zeros, ft) {
              True -> list.range(1, 9)
              False -> list.range(0, 9)
            }
            |> set.from_list
            |> set.difference(used)

          rem
          |> set.map(fn(i) { ac |> dict.insert(ft, i) })
          |> set.to_list
        })
        |> list.flatten

      completer(new_acc, texts |> list.drop(1), no_zeros, puzzle)
    }
    _ -> {
      acc
      |> list.filter(fn(l) {
        let parsed =
          l
          |> dict.fold(puzzle, fn(acc, k, v) {
            acc |> string.replace(k, v |> int.to_string)
          })

        let assert [left, right] = parsed |> string.split(" == ")
        left
        |> string.split(" + ")
        |> list.fold(0, fn(acc, cur) {
          acc + { cur |> int.parse |> result.unwrap(0) }
        })
        == int.parse(right) |> result.unwrap(0)
      })
    }
  }
}

pub fn main() {
  echo solve("SEND + MORE == MONEY")
}
