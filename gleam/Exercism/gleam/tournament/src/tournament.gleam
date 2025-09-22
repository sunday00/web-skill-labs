import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

type Match {
  Match(mp: Int, w: Int, d: Int, l: Int, p: Int)
}

pub fn tally(input: String) -> String {
  let head = string.pad_end("Team", 31, " ") <> "| MP |  W |  D |  L |  P"

  case input |> string.length == 0 {
    True -> head
    False -> {
      let r: Dict(String, Match) = dict.new()
      let r = reducer(r, input |> string.split("\n"))

      let r =
        r
        |> dict.to_list
        |> list.sort(by: fn(a, b) { int.compare({ b.1 }.p, { a.1 }.p) })
        |> list.map(fn(el) {
          let #(k, v) = el
          k |> string.pad_end(30, " ")
          <> " | "
          <> v.mp |> int.to_string |> string.pad_start(2, " ")
          <> " | "
          <> v.w |> int.to_string |> string.pad_start(2, " ")
          <> " | "
          <> v.d |> int.to_string |> string.pad_start(2, " ")
          <> " | "
          <> v.l |> int.to_string |> string.pad_start(2, " ")
          <> " | "
          <> v.p |> int.to_string |> string.pad_start(2, " ")
        })

      head <> "\n" <> { r |> string.join("\n") }
    }
  }
}

fn reducer(acc: Dict(String, Match), rem: List(String)) {
  case rem {
    [] -> acc
    [f, ..r] -> {
      let assert [target, enemy, res] = string.split(f, ";")
      // echo #(target, enemy, res)

      reducer(
        acc
          |> dict.upsert(target, fn(el) { update_result_table(el, res) })
          |> dict.upsert(enemy, fn(el) {
            let reverse_res = reverse_res(res)

            update_result_table(el, reverse_res)
          }),
        r,
      )
    }
  }
}

fn reverse_res(r: String) {
  case r {
    "win" -> "loss"
    "draw" -> "draw"
    "loss" -> "win"
    _ -> "?"
  }
}

fn update_result_table(match: Option(Match), res: String) {
  case match {
    Some(x) -> {
      case res {
        "win" -> Match(..x, mp: x.mp + 1, w: x.w + 1, p: x.p + 3)
        "draw" -> Match(..x, mp: x.mp + 1, d: x.d + 1, p: x.p + 1)
        "loss" -> Match(..x, mp: x.mp + 1, l: x.l + 1, p: x.p + 0)
        _ -> x
      }
    }
    None -> {
      case res {
        "win" -> Match(1, 1, 0, 0, 3)
        "draw" -> Match(1, 0, 1, 0, 1)
        "loss" -> Match(1, 0, 0, 1, 0)
        _ -> Match(0, 0, 0, 0, 0)
      }
    }
  }
}

pub fn main() {
  tally(
    "Allegoric Alaskans;Blithering Badgers;win
Blithering Badgers;Courageous Californians;win
Courageous Californians;Allegoric Alaskans;loss",
  )
}
