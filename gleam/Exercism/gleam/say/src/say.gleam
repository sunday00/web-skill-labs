import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string

pub type Error {
  OutOfRange
}

pub fn say(number: Int) -> Result(String, Error) {
  let speakers =
    dict.from_list([
      #(1, "one"),
      #(2, "two"),
      #(3, "three"),
      #(4, "four"),
      #(5, "five"),
      #(6, "six"),
      #(7, "seven"),
      #(8, "eight"),
      #(9, "nine"),
      #(10, "ten"),
      #(11, "eleven"),
      #(12, "twelve"),
      #(13, "thirteen"),
      #(14, "fourteen"),
      #(15, "fifteen"),
      #(16, "sixteen"),
      #(17, "seventeen"),
      #(18, "eighteen"),
      #(19, "nineteen"),
      #(20, "twenty"),
      #(30, "thirty"),
      #(40, "forty"),
      #(50, "fifty"),
      #(60, "sixty"),
      #(70, "seventy"),
      #(80, "eighty"),
      #(90, "ninety"),
    ])

  case number {
    n if n < 0 -> Error(OutOfRange)
    n if n == 0 -> Ok("zero")
    n if n < 100 -> under_hundred(n, speakers)
    n if n < 1_000_000_000_000 -> {
      let ls = reducer([], n, speakers)

      ls
      |> string.join(" ")
      |> string.replace("  ", " ")
      |> string.trim_start
      |> string.trim_end
      |> Ok
    }
    _ -> Error(OutOfRange)
  }
}

fn under_hundred(n: Int, speakers: Dict(Int, String)) {
  case n {
    n if n <= 20 -> speakers |> dict.get(n) |> result.unwrap("") |> Ok
    n if n < 100 -> {
      let k = { n / 10 } * 10
      let r = n % 10

      let ks = speakers |> dict.get(k) |> result.unwrap("")
      let rs = speakers |> dict.get(r) |> result.unwrap("")

      case rs |> string.length {
        sl if sl > 0 -> { ks <> "-" <> rs } |> Ok
        _ -> ks |> Ok
      }
    }
    _ -> Error(OutOfRange)
  }
}

fn reducer(acc: List(String), rem: Int, speakers: Dict(Int, String)) {
  case rem < 1 {
    True -> acc
    False -> {
      case acc |> list.length {
        0 -> {
          let r = rem / 100

          let cook = rem % 100
          case cook {
            0 -> reducer(acc |> list.prepend(" "), r, speakers)
            _ -> {
              let assert Ok(coor) = under_hundred(cook, speakers)

              reducer(acc |> list.prepend(coor), r, speakers)
            }
          }
        }
        1 -> {
          let k = rem % 10
          let r = rem / 10

          case k == 0 {
            True -> {
              reducer(acc |> list.prepend(" "), r, speakers)
            }
            False -> {
              let assert Ok(h) = speakers |> dict.get(k)

              reducer(acc |> list.prepend(h <> " hundred"), r, speakers)
            }
          }
        }
        l if 2 <= l && l <= 4 -> {
          let tmb = case acc |> list.length {
            2 -> " thousand"
            3 -> " million"
            _ -> " billion"
          }

          let r = rem / 1000
          let k = rem % 1000
          let rhk = k / 100
          let rrs = k % 100

          let rh = case rhk == 0 {
            True -> " "
            False ->
              speakers |> dict.get(rhk) |> result.unwrap("") <> " hundred "
          }
          let rr = case rrs == 0 {
            True -> " "
            False ->
              under_hundred(rrs, speakers) |> result.unwrap("") <> " " <> tmb
          }

          reducer(acc |> list.prepend(rh <> rr), r, speakers)
        }
        _ -> acc
      }
    }
  }
}
