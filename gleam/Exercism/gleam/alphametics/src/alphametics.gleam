import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/set
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

  let parsed =
    resolve(all_letters, no_zeros |> list.unique, tails, puzzle, right)

  case parsed |> list.length > 0 {
    True -> parsed |> list.first
    False -> Error(Nil)
  }
}

fn resolve(
  all_letters: List(String),
  no_zeros: List(String),
  lasts_letters: List(String),
  puzzle: String,
  right: String,
) {
  let answer_last = right |> string.last |> result.unwrap("")

  let zerobles =
    all_letters |> set.from_list |> set.difference(no_zeros |> set.from_list)
  let last_combinations = case zerobles |> set.size == 1 {
    True -> {
      let assert Ok(z) = zerobles |> set.to_list |> list.first
      lasts(
        [dict.from_list([#(z, 0)])],
        no_zeros,
        lasts_letters |> list.append([answer_last]),
        lasts_letters,
        answer_last,
      )
    }
    False ->
      lasts(
        [],
        no_zeros,
        lasts_letters |> list.append([answer_last]),
        lasts_letters,
        answer_last,
      )
  }

  let feel_texts =
    all_letters |> set.from_list |> set.difference(set.from_list(lasts_letters))

  completer(last_combinations, feel_texts |> set.to_list, no_zeros, puzzle)
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
  echo solve(
    "THIS + A + FIRE + THEREFORE + FOR + ALL + HISTORIES + I + TELL + A + TALE + THAT + FALSIFIES + ITS + TITLE + TIS + A + LIE + THE + TALE + OF + THE + LAST + FIRE + HORSES + LATE + AFTER + THE + FIRST + FATHERS + FORESEE + THE + HORRORS + THE + LAST + FREE + TROLL + TERRIFIES + THE + HORSES + OF + FIRE + THE + TROLL + RESTS + AT + THE + HOLE + OF + LOSSES + IT + IS + THERE + THAT + SHE + STORES + ROLES + OF + LEATHERS + AFTER + SHE + SATISFIES + HER + HATE + OFF + THOSE + FEARS + A + TASTE + RISES + AS + SHE + HEARS + THE + LEAST + FAR + HORSE + THOSE + FAST + HORSES + THAT + FIRST + HEAR + THE + TROLL + FLEE + OFF + TO + THE + FOREST + THE + HORSES + THAT + ALERTS + RAISE + THE + STARES + OF + THE + OTHERS + AS + THE + TROLL + ASSAILS + AT + THE + TOTAL + SHIFT + HER + TEETH + TEAR + HOOF + OFF + TORSO + AS + THE + LAST + HORSE + FORFEITS + ITS + LIFE + THE + FIRST + FATHERS + HEAR + OF + THE + HORRORS + THEIR + FEARS + THAT + THE + FIRES + FOR + THEIR + FEASTS + ARREST + AS + THE + FIRST + FATHERS + RESETTLE + THE + LAST + OF + THE + FIRE + HORSES + THE + LAST + TROLL + HARASSES + THE + FOREST + HEART + FREE + AT + LAST + OF + THE + LAST + TROLL + ALL + OFFER + THEIR + FIRE + HEAT + TO + THE + ASSISTERS + FAR + OFF + THE + TROLL + FASTS + ITS + LIFE + SHORTER + AS + STARS + RISE + THE + HORSES + REST + SAFE + AFTER + ALL + SHARE + HOT + FISH + AS + THEIR + AFFILIATES + TAILOR + A + ROOFS + FOR + THEIR + SAFE == FORTRESSES",
  )
}
