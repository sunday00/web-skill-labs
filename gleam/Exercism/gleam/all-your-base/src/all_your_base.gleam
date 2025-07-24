import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Error {
  InvalidBase(Int)
  InvalidDigit(Int)
}

pub fn rebase(
  digits digits: List(Int),
  input_base input_base: Int,
  output_base output_base: Int,
) -> Result(List(Int), Error) {
  case digits, input_base, output_base {
    _, i, _ if i < 2 -> Error(InvalidBase(i))
    _, _, o if o < 2 -> Error(InvalidBase(o))
    digit, i, _ -> {
      case digit |> list.find(fn(d) { d < 0 || d >= i }) {
        Ok(n) -> Error(InvalidDigit(n))
        _ -> {
          let tens = case i {
            i if i == 10 ->
              digit
              |> list.map(fn(n) { n |> int.to_string })
              |> string.concat
              |> int.parse
              |> result.unwrap(0)
            _ -> to_ten(digit, i)
          }

          Ok(to_out([], tens, output_base, tens))
        }
      }
    }
  }
}

fn to_ten(digis: List(Int), inp: Int) {
  digis
  |> list.reverse
  |> list.index_map(fn(n, i) {
    n
    * {
      int.power(inp, i |> int.to_float) |> result.unwrap(0.0) |> float.truncate
    }
  })
  |> list.reduce(fn(acc, cur) { acc + cur })
  |> result.unwrap(0)
}

fn to_out(acc: List(Int), tens: Int, out: Int, cur: Int) {
  case cur {
    c if c >= out -> {
      to_out(acc |> list.prepend(c % out), tens, out, c / out)
    }
    _ -> {
      acc |> list.prepend(cur)
    }
  }
}

pub fn main() {
  rebase([4, 2], 10, 2)
}
