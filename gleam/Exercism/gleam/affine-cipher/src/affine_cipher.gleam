import gleam/float
import gleam/int
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string

pub type Error {
  KeyNotCoprime(Int, Int)
}

const abc = "abcdefghijklmnopqrstuvwxyz"

pub fn encode(
  plaintext plaintext: String,
  a a: Int,
  b b: Int,
) -> Result(String, Error) {
  case a % 2 == 0 || a % 13 == 0 {
    True -> Error(KeyNotCoprime(a, string.length(abc)))
    False -> {
      let assert Ok(patt) = regexp.from_string("[a-z0-9]")

      let li =
        plaintext
        |> string.lowercase
        |> string.replace(" ", "")
        |> string.to_graphemes
        |> list.filter(fn(ch) { regexp.check(patt, ch) })
        |> list.map(to_i)

      let lch = li |> list.map(fn(i) { to_ch(i, a, b) })

      string_group_5([], [], lch) |> string.trim |> Ok
    }
  }
}

fn to_i(char: String) {
  let abc_i = abc |> string.to_graphemes |> list.index_map(fn(s, i) { #(s, i) })

  case char |> int.parse {
    Ok(n) -> -1 * n
    _ -> abc_i |> list.key_find(char) |> result.unwrap(-1)
  }
}

fn to_ch(i: Int, a: Int, b: Int) {
  let i_abc = abc |> string.to_graphemes |> list.index_map(fn(s, i) { #(i, s) })

  case i >= 0 {
    True -> {
      let cal = a * i + b
      let cal = cal % string.length(abc)

      i_abc |> list.key_find(cal) |> result.unwrap("")
    }
    _ -> {
      i * -1 |> int.to_string
    }
  }
}

fn string_group_5(acc: List(String), cur: List(String), rem: List(String)) {
  case rem {
    [f, ..r] -> {
      case list.length(cur) {
        4 -> {
          string_group_5(
            acc
              |> list.prepend(
                cur |> list.prepend(f) |> list.reverse |> string.join(""),
              ),
            [],
            r,
          )
        }
        _ -> {
          string_group_5(acc, cur |> list.prepend(f), r)
        }
      }
    }
    [] -> {
      acc
      |> list.prepend(cur |> list.reverse |> string.join(""))
      |> list.reverse
      |> string.join(" ")
    }
  }
}

pub fn decode(
  ciphertext ciphertext: String,
  a a: Int,
  b b: Int,
) -> Result(String, Error) {
  case a % 2 == 0 || a % 13 == 0 {
    True -> Error(KeyNotCoprime(a, string.length(abc)))
    False -> {
      let li =
        ciphertext
        |> string.replace(" ", "")
        |> string.to_graphemes
        |> list.map(to_i)

      let lch = li |> list.map(fn(i) { dec_each(i, a, b) })

      let i_abc =
        abc |> string.to_graphemes |> list.index_map(fn(s, i) { #(i, s) })

      lch
      |> list.map(fn(i) {
        case i >= 0 {
          True -> i_abc |> list.key_find(i) |> result.unwrap("")
          False -> -1 * i |> int.to_string
        }
      })
      |> string.join("")
      |> Ok
    }
  }
}

fn dec_each(i: Int, a: Int, b: Int) {
  case i >= 0 {
    True -> {
      let index = mmi(a) * { i - b } % { abc |> string.length }

      case index >= 0 {
        True -> index
        False -> { index + { -index / 26 + 1 } * 26 } % 26
      }
    }
    False -> i
  }
}

fn mmi(i: Int) {
  let x = mmi_reducer(i, 26, 1, 0)

  case x < 0 {
    True -> x + 26
    False -> x
  }
}

fn mmi_reducer(i: Int, m: Int, x: Int, y: Int) {
  case i > 1 {
    True -> {
      let q =
        float.floor({ i |> int.to_float } /. { m |> int.to_float })
        |> float.round
      let t = m

      let new_m = i % m
      let new_i = t
      let t = y

      let y = x - q * y
      let x = t

      // echo i
      mmi_reducer(new_i, new_m, x, y)
    }
    False -> {
      x
    }
  }
}

pub fn main() {
  // echo decode("qdwju nqcro muwhn odqun oppmd aunwd o", 19, 16)
  echo decode("tytgn fjr", 3, 7)
  // echo mmi(15)

  // echo mmi_reducer(7, 13, 1, 0)
}
