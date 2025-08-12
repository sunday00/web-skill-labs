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

  let answer_last = right |> string.last |> result.unwrap("")
  let parsed = lasts([], all_letters, tails, answer_last)

  Ok(dict.from_list([]))
}

fn lasts(
  acc: List(List(#(String, Int))),
  all_letters: List(String),
  lasts: List(String),
  answer_last: String,
) {
  echo combinations([], lasts, list.range(0, 9) |> set.from_list)
}

fn combinations(
  acc: List(List(#(String, Int))),
  texts: List(String),
  avail: Set(Int),
) -> List(List(#(String, Int))) {
  case acc |> list.length {
    0 -> {
      let assert Ok(f) = texts |> list.first

      combinations(
        avail |> set.map(fn(i) { [#(f, i)] }) |> set.to_list,
        texts |> list.drop(1),
        avail,
      )
    }
    _ -> {
      let f = texts |> list.first

      case f {
        Ok(ft) -> {
          let new_acc =
            acc
            |> list.map(fn(ac) {
              let used =
                ac
                |> list.map(fn(a) {
                  let #(_, i) = a
                  i
                })
                |> set.from_list

              let new_avail = avail |> set.difference(used)

              new_avail
              |> set.map(fn(i) { ac |> list.append([#(ft, i)]) })
              |> set.to_list
            })
            |> list.flatten

          combinations(new_acc, texts |> list.drop(1), avail)
        }
        _ -> {
          process.sleep(1000)
          echo acc
        }
      }
    }
  }
}

pub fn main() {
  echo solve("AND + A + STRONG + OFFENSE + AS + A + GOOD == DEFENSE")
}
